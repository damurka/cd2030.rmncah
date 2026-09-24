service_utilization_capita_indicators <- c('opd', 'ipd')

utilization_capita_ui <- function(id, i18n, title_key) {
  ns <- NS(id)
  
  cd_tabbed_charts_ui(ns("panel"), i18n, "title_subnational_utilization", cd_coverage_plot_ui, 
              indicators = service_utilization_capita_indicators, showCustom = FALSE)
}

utilization_capita_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
          data_rx <- reactive({
            req(cache(), active())
            cache()$generate_admin1_service_utilization(current_indicator)
          })
          
          cd_coverage_plot_server(
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
        indicators = service_utilization_capita_indicators,
        showCustom = FALSE
      )
    }
  )
}