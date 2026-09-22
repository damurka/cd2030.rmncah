source("modules/6_national_service_utilization/service_utilization.R")

utilization_map_indicators <- c('opd_map', 'ipd_map')

nationalServiceUtilizationUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('service_utilization'),
    dashboardTitle = i18n$t("title_national_utilization"),
    i18n = i18n,

    countdownOptions = countdownOptions(
      title = i18n$t('title_global_options'),
      column(3, cdChipMulti(ns('years'), "title_global_select_years", i18n = i18n))
    ),
    
    utilizationUI(ns('national'), i18n, 'title_national_utilization'),

    tagList(
      tabPanelsUI(ns("panel"), i18n, "title_national_utilization", downloadCoverageUI, 
                indicators = utilization_map_indicators, showCustom = FALSE),
      
    )
    
  )
}

nationalServiceUtilizationServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      yearsSelectSync(input, session, "years",
        years = reactive({ req(cache()); cache()$data_years }),
        selected = reactive({ req(cache()); cache()$utilization_mapping_years })
      )
      
      observeEvent(input$years, {
        req(cache())
        cache()$set_utilization_mapping_years(as.integer(input$years))
      })
      
      utilizationServer('national', cache, i18n, 'national')

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          ind_map <- reactive({
            req(cache())
            ind <- gsub('_map$', '', current_indicator)
            cache()$prepare_mapping_service_utlization(ind)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = ind_map,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            i18n = i18n
          )
        },
        indicators = utilization_map_indicators
      )

      countdownHeaderServer(
        'service_utilization',
        cache = cache,
        path = '10-service-utilisation',
        i18n = i18n
      )
    }
  )
}
