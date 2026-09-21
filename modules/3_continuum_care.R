continuum_indicators <- c('maternal_continuum', 'child_continuum')

continuumCoverageUI <- function(id, i18n, label) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("national_coverage"),
    dashboardTitle = i18n$t("title_continuum"),
    i18n = i18n,
    include_report = TRUE,
    tabPanelsUI(ns("panel"), i18n, 'title_national_coverage', downloadCoverageUI, indicators = continuum_indicators, showCustom = FALSE),
    tabPanelsUI(ns("panel1"), i18n, 'title_national_coverage', downloadCoverageUI, indicators = continuum_indicators, showCustom = FALSE)
  )
}

continuumCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          coverage_data <- reactive({
            req(cache())
            indic <- str_remove(current_indicator, '_continuum')
            cache()$generate_coverage_data('national', indic)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive("continuum_care"),
            data_fn = coverage_data,
            sheet_name = reactive(i18n$t("continuum_care")),
            plot_fun = function(d) {
              plot(
                d, 
                type = 'profile',
                    
                    # Dynamic titles based on whether it's 'profile' or 'gap'
                title    = i18n$t("title_cov_profile"),
                subtitle = i18n$t("subtitle_cov_profile"),
                    
                    # Dynamic axes
                x_axis   = NULL,
                y_axis   = i18n$t("opt_coverage"),
                    
                    # Translation lists
                source_labels = list(
                  facility = i18n$t("lbl_src_facility"),
                  survey   = i18n$t("lbl_src_survey"),
                  wuenic   = i18n$t("lbl_src_wuenic")
                ),
                indicator_labels = list(
                  anc_1trimester = i18n$t("opt_anc_1trimester"),
                  anc4           = i18n$t("opt_anc4"),
                  ideliv = i18n$t("opt_ideliv"),
                  instlivebirths = i18n$t("opt_instlivebirths"),
                  pnc48h         = i18n$t("opt_pnc48h"),
                  penta3         = i18n$t("opt_penta3"),
                  measles1       = i18n$t("opt_measles1")
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = continuum_indicators
      )

      tabPanelsServer(
        "panel1",
        serverInput = function(id, current_indicator) {
          subnational_coverage_data <- reactive({
            req(cache())
            indic <- str_remove(current_indicator, '_continuum')
            cache()$generate_coverage_data('adminlevel_1', indic)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive("continuum_care"),
            data_fn = subnational_coverage_data,
            sheet_name = reactive(i18n$t("continuum_care")),
            plot_fun = function(d) {
              plot(
                d, 
                type = 'heatmap',
                    
                    # Dynamic titles based on whether it's 'profile' or 'gap'
                title    = i18n$t("title_cov_profile"),
                subtitle = i18n$t("subtitle_cov_profile"),
                    
                    # Dynamic axes
                x_axis   = NULL,
                y_axis   = i18n$t("opt_coverage"),
                    
                    # Translation lists
                source_labels = list(
                  facility = i18n$t("lbl_src_facility"),
                  survey   = i18n$t("lbl_src_survey"),
                  wuenic   = i18n$t("lbl_src_wuenic")
                ),
                indicator_labels = list(
                  anc_1trimester = i18n$t("opt_anc_1trimester"),
                  anc4           = i18n$t("opt_anc4"),
                  ideliv = i18n$t("opt_ideliv"),
                  instlivebirths = i18n$t("opt_instlivebirths"),
                  pnc48h         = i18n$t("opt_pnc48h"),
                  penta3         = i18n$t("opt_penta3"),
                  measles1       = i18n$t("opt_measles1")
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = continuum_indicators
      )
      
      countdownHeaderServer(
        "national_coverage",
        cache = cache,
        path = "5-coverage-estimation",
        i18n = i18n
      )
    }
  )
}