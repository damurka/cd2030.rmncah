mortalityMappingUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_mapping'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, selectizeInput(ns('years'), label = i18n$t("title_select_years"), choice = NULL, multiple = TRUE))
    ),

    tabBox(
      title = i18n$t('title_mortality_mapping'),
      width = 12,

      tabPanel(title = i18n$t("opt_mmr_inst"), downloadCoverageUI(ns('mmr_inst'))),
      tabPanel(title = i18n$t("opt_sbr_inst"), downloadCoverageUI(ns('sbr_inst')))
    )
  )
}

mortalityMappingServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      iso <- reactive({
        req(data())
        attr_or_abort(data(), 'iso3')
      })

      mortality_summary <- reactive({
        req(cache(), cache()$check_mortality_params)
        cache()$create_mortality_summary()
      })

      mmr_inst <- reactive({
        req(mortality_summary())
        cache()$filter_mortality_summary(mortality_summary(), 'mmr')
      })

      sbr_inst <- reactive({
        req(mortality_summary())
        cache()$filter_mortality_summary(mortality_summary(), 'sbr')
      })

      observe({
        req(cache(), mortality_summary())

        survey_years <- mortality_summary() %>%
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

      downloadCoverageServer(
        id = 'mmr_inst',
        filename = reactive(paste0('mmr_inst_')),
        data_fn = mmr_inst,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_mmr_inst'))
      )

      downloadCoverageServer(
        id = 'sbr_inst',
        filename = reactive(paste0('sbr_inst')),
        data_fn = sbr_inst,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_inst'))
      )

      contentHeaderServer(
        'mortality',
        cache = cache,
        path = 'maternal-mortality',
        i18n = i18n
      )
    }
  )
}
