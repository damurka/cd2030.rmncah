bayesian_indicators <- c('anc4', 'anc_1trimester', 'ideliv', 'measles1', 'penta3')

bayesianUI <- function(id, i18n, label) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("bayesian_analysis"),
    dashboardTitle = i18n$t("title_bayesian_analysis"),
    i18n = i18n,
    tabPanelsUI(ns("panel"), i18n, "title_bayesian_analysis", downloadCoverageUI,
      indicators = bayesian_indicators
    )
  )
}

bayesianServer <- function(id, cache, i18n, admin_level) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {

          model <- reactive({
            req(cache())
            cache()$get_bayes_model(admin_level, current_indicator)
          })

          downloadCoverageServer(
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
      
      countdownHeaderServer(
        "bayesian_analysis",
        cache = cache,
        path = "5.1-bayesian-coverage",
        i18n = i18n
      )
    }
  )
}