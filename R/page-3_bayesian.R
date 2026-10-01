bayesian_indicators <- c('anc4', 'anc_1trimester', 'ideliv', 'measles1', 'penta3')

bayesian_ui <- function(id, i18n, label) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    # the model's packages are installed when it is first wanted (see bayesian_server)
    uiOutput(ns("needs_packages")),
    cd_tabbed_charts_ui(ns("panel"), i18n, "title_bayesian_analysis", cd_coverage_plot_ui,
      indicators = bayesian_indicators
    )
  )
}

bayesian_server <- function(id, cache, i18n, admin_level, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      # The model's packages (Stan and its packages: a large download not every analysis needs) are installed on
      # demand. Inside DataSuite a button asks it to install them (then it offers to restart the app); in plain R
      # the page shows the command. No fit is started while they are missing.
      missing_packages <- cd2030.core::cd_bayes_packages_missing()
      output$needs_packages <- renderUI({
        if (!length(missing_packages)) return(NULL)
        in_datasuite <- nzchar(Sys.getenv("CDSUITE_SHINY_ID"))
        div(
          class = "alert alert-info",
          p(i18n$t("msg_bayes_needs_packages")),
          if (in_datasuite) {
            actionButton(ns("install_packages"), i18n$t("btn_bayes_install_packages"), icon = icon("download"))
          } else {
            tags$pre(cd2030.core::cd_bayes_install_command(missing_packages))
          }
        )
      })
      observeEvent(input$install_packages, {
        cd2030.core::cd_request_bayes_packages(missing_packages)
        showNotification(i18n$t("msg_bayes_installing_packages"), type = "message")
      })

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {

          # A fit (rstan) takes minutes: it runs in another R process (an ExtendedTask with future), so the app stays
          # usable while it runs and the chart shows its loader meanwhile. Each result comes back with the key it was
          # fitted for (admin level, indicator, denominator) and is kept in the dataset under it, so a fit that ends
          # after the denominator changed is kept for the one it was asked for, not shown for the new one.
          fit <- ExtendedTask$new(function(inputs) {
            promises::future_promise({
              list(key = inputs$key, model = cd2030.core::generate_bayes_model(
                coverage_data = inputs$coverage_data,
                overall_score = inputs$overall_score,
                indicator = inputs$indicator,
                denominator = inputs$denominator
              ))
            }, seed = TRUE, packages = "cd2030.core")
          })
          asked <- reactiveVal(NULL)

          # Shiny computes every bound output once on a session's first flush, before it has heard back from
          # the client about which ones are actually visible -- so without req(active()), a fit would start for
          # every indicator, at both admin levels, for a page no one has opened yet. active() (page_is(), see
          # app.R) keeps it from starting until this tab is actually open; and it starts only when the chart asks
          # for the model (the indicator's tab shown), the same fix mortality_mapping_server() already needed.
          model <- reactive({
            req(cache(), active())
            validate(need(!length(missing_packages), i18n$t("msg_bayes_needs_packages")))
            dataset <- cache()
            done <- dataset$bayes_model_cached(admin_level, current_indicator)
            if (!is.null(done)) return(done)
            key <- dataset$bayes_model_key(admin_level, current_indicator)
            if (!identical(isolate(asked()), key)) {
              isolate({
                asked(key)
                fit$invoke(dataset$bayes_model_inputs(admin_level, current_indicator))
              })
            }
            # running: the output stays busy (the chart's loader); an error is shown as the chart's
            out <- fit$result()
            dataset$keep_bayes_model(out$key, out$model)
            req(identical(out$key, key))
            out$model
          })

          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_bayesian")),
            data_fn = model,
            sheet_name = reactive('bayesian'),
            plot_fun = function(d) {
             nice_indicator <- i18n$t(paste0('opt_', current_indicator))
              
              plot(
                d,
                # Dynamically paste the translated plot title and the translated indicator name
                title = paste(i18n$t("plot_title_bayes_coverage"), "-", nice_indicator),
                x_axis = i18n$t("title_global_year"),
                y_axis = i18n$t("opt_coverage"),
                caption = i18n$t("plot_legend_source")
              )
            },
            i18n = i18n
          )
        },
        indicators = bayesian_indicators
      )
      
    }
  )
}