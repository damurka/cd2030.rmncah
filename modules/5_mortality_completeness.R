mort_compl_indicators <- c('mmr_ratio', 'sbr_ratio')

mortalityCompletenessUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_completeness'),
    i18n = i18n,
    include_report = TRUE,
    tabPanelsUI(ns("panel"), i18n, "title_mortality_completeness", downloadCoverageUI, 
                indicators = mort_compl_indicators, showCustom = FALSE)
  )
}

mortalityCompletenessServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          data_rx <- reactive({
            req(cache(), cache()$check_inequality_params)
            cache()$summarise_completeness_ratio(str_remove(current_indicator, '_ratio'))
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) plot(d),
            i18n = i18n
          )
        },
        indicators = mort_compl_indicators
      )

      countdownHeaderServer(
        'mortality',
        cache = cache,
        path = '9-mortality',
        i18n = i18n
      )
    }
  )
}
