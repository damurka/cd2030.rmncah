mort_map_indicators <- c('mmr_inst', 'sbr_inst')

mortalityMappingUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_mapping'),
    i18n = i18n,

    countdownOptions = countdownOptions(
      title = i18n$t('title_global_options'),
      column(3, selectizeInput(ns('years'), label = i18n$t("title_global_select_years"), choice = NULL, multiple = TRUE))
    ),
    
    tabPanelsUI(ns("panel"), i18n, "title_mortality_mapping", downloadCoverageUI, 
                indicators = mort_map_indicators, showCustom = FALSE)
  )
}

mortalityMappingServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          data_rx <- reactive({
            req(cache(), cache()$check_mortality_params)
            cache()$filter_mortality_summary(str_remove(current_indicator, '_inst'))
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
        indicators = mort_map_indicators
      )

      observe({
        req(cache(), cache()$mortality_summary)

        survey_years <- cache()$mortality_summary %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        survey_years <- c('All years' = '', survey_years)
        updateSelectizeInput(session, 'years', choices = survey_years, selected = cache()$mortality_mapping_years)
      })

      observeEvent(input$years, {
        req(cache())
        cache()$set_mortality_mapping_years(as.integer(input$years))
      })

      countdownHeaderServer(
        'mortality',
        cache = cache,
        path = '9-mortality',
        i18n = i18n
      )
    }
  )
}
