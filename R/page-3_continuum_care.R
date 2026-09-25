continuum_indicators <- c('maternal_continuum', 'child_continuum')

continuum_coverage_ui <- function(id, i18n, label) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_tabbed_charts_ui(ns("panel"), i18n, 'title_national_coverage', cd_coverage_plot_ui, indicators = continuum_indicators, showCustom = FALSE),
    cd_tabbed_charts_ui(ns("panel1"), i18n, 'title_national_coverage', cd_coverage_plot_ui, indicators = continuum_indicators, showCustom = FALSE)
  )
}

continuum_coverage_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          coverage_data <- reactive({
            req(cache(), active())
            indic <- str_remove(current_indicator, '_continuum')
            cache()$generate_coverage_data('national', indic)
          })
          
          cd_coverage_plot_server(
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
        indicators = continuum_indicators,
        showCustom = FALSE
      )

      cd_tabbed_charts_server(
        "panel1",
        serverInput = function(id, current_indicator) {
          subnational_coverage_data <- reactive({
            req(cache(), active())
            indic <- str_remove(current_indicator, '_continuum')
            cache()$generate_coverage_data('adminlevel_1', indic)
          })
          
          cd_coverage_plot_server(
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
        indicators = continuum_indicators,
        showCustom = FALSE
      )
      
    }
  )
}