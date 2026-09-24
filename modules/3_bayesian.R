bayesian_indicators <- c('anc4', 'anc_1trimester', 'ideliv', 'measles1', 'penta3')

bayesian_ui <- function(id, i18n, label) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
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

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {

          # Shiny computes every bound output once on a session's first flush, before it has heard back from
          # the client about which ones are actually visible -- so without req(active()), fitting a Bayesian
          # model (rstan, tens of seconds each) runs for every indicator, at both admin levels, for a page no
          # one has opened yet, on every session. active() (page_is(), see app.R) keeps it from starting until
          # this tab is actually open, the same fix mortality_mapping_server() already needed.
          model <- reactive({
            req(cache(), active())
            cache()$get_bayes_model(admin_level, current_indicator)
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