service_utilization_indicators <- c('opd', 'ipd', 'under5', 'cfr', 'deaths')

utilizationUI <- function(id, i18n, title_key) {
  ns <- NS(id)
  
  tabPanelsUI(ns("panel"), i18n, "title_national_utilization", downloadCoverageUI, 
              indicators = service_utilization_indicators, showCustom = FALSE)
}

utilizationServer <- function(id, cache, i18n, admin_level, region = reactive(NULL)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(region))
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          data_rx <- reactive({
            req(cache())
            dt <- cache()$filter_service_utilization(admin_level, current_indicator, region())
            req(dt)
            return(dt)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, '_utilization')),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) plot(d),
            i18n = i18n
          )
        },
        indicators = service_utilization_indicators
      )
    }
  )
}