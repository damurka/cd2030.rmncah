service_utilization_capita_indicators <- c('opd', 'ipd')

utilizationCapitaUI <- function(id, i18n, title_key) {
  ns <- NS(id)
  
  tabPanelsUI(ns("panel"), i18n, "title_subnational_utilization", downloadCoverageUI, 
              indicators = service_utilization_capita_indicators, showCustom = FALSE)
}

utilizationCapitaServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          data_rx <- reactive({
            req(cache())
            cache()$generate_admin1_service_utilization(current_indicator)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, '_utilization_capita')),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) {
              plot(
                d, 
                title = i18n$t(paste0('opt_', current_indicator, '_capita_title')),
                x_axis = i18n$t(paste0('opt_', current_indicator, '_capita_x_axis')),
                legend = i18n$t(paste0('opt_', current_indicator, '_capita_legend')),
              )
            },
            i18n = i18n
          )
        },
        indicators = service_utilization_capita_indicators
      )
    }
  )
}