service_utilization_indicators <- c('opd', 'ipd', 'under5', 'cfr', 'deaths')

utilization_ui <- function(id, i18n, title_key) {
  ns <- NS(id)
  
  cd_tabbed_charts_ui(ns("panel"), i18n, "title_national_utilization", cd_coverage_plot_ui, 
              indicators = service_utilization_indicators, showCustom = FALSE)
}

utilization_server <- function(id, cache, i18n, admin_level, region = reactive(NULL), active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(region))
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
            dt <- cache()$filter_service_utilization(admin_level, current_indicator, region())
            req(dt)
            return(dt)
          })
          
          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, '_utilization')),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) plot(d),
            i18n = i18n
          )
        },
        indicators = service_utilization_indicators,
        showCustom = FALSE
      )
    }
  )
}