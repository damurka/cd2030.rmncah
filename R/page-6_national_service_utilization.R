
utilization_map_indicators <- c('opd_map', 'ipd_map')

national_service_utilization_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    utilization_ui(ns('national'), i18n, 'title_national_utilization'),

    tagList(
      cd_map_options(
        cd_chip_multi(ns('years'), "title_global_select_years", i18n = i18n),
        cd_palette_chip(ns("palette"), i18n, first = "Purples")
      ),
      cd_tabbed_charts_ui(ns("panel"), i18n, "title_national_utilization", cd_coverage_plot_ui, 
                indicators = utilization_map_indicators, showCustom = FALSE),
      
    )
    
  )
}

national_service_utilization_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      cd_years_sync(input, session, "years",
        years = reactive({ req(cache()); cache()$data_years }),
        selected = reactive({ req(cache()); cache()$utilization_mapping_years })
      )
      
      observeEvent(input$years, {
        req(cache())
        cache()$set_utilization_mapping_years(cd_years_input(input$years, cache()$data_years))
      })
      
      utilization_server('national', cache, i18n, 'national', active = active)

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
          ind_map <- reactive({
            req(cache(), active(), input$palette)
            ind <- gsub('_map$', '', current_indicator)
            cache()$prepare_mapping_service_utlization(ind, palette = input$palette)
          })
          
          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = ind_map,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            i18n = i18n
          )
        },
        indicators = utilization_map_indicators,
        showCustom = FALSE
      )

    }
  )
}
