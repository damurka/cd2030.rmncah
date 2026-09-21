survey_comp_indicators <- c("instlivebirths", "penta3")

surveyComparisonUI <- function(id, i18n) {
  ns <- NS(id)
  tabPanelsUI(ns("panel"), 
              i18n, 
              "title_coverage_national",
              uiInput = downloadCoverageUI,
              indicators = survey_comp_indicators, 
              showCustom = FALSE
  )
}

surveyComparisonServer <- function(id, cache, admin_level, region, i18n) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(admin_level)) 
  stopifnot(is.reactive(region))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      denominatorInputServer("denominator", cache, i18n)

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          coverage <- reactive({
            req(cache(), cache()$check_inequality_params, admin_level())
            cache()$calculate_derived_coverage(current_indicator, admin_level(), region())
          })

          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_plot")),
            data_fn = coverage,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              indicator <- i18n$t(paste0("opt_", current_indicator))
              year <- cache()$survey_year
              plot(d,
                   title = str_glue(i18n$t("plt_title_denom_survey_comp")),
                   y_label = str_glue(i18n$t("lbl_axis_y_coverage")),
                   category_labels = list(
                     un            = i18n$t("lbl_denom_un_proj"),
                     dhis2         = i18n$t("lbl_denom_dhis2_proj"),
                     anc1          = i18n$t("lbl_denom_anc1_derived"),
                     penta1        = i18n$t("lbl_denom_penta1_derived"),
                     penta1derived = i18n$t("opt_penta1derived"),
                     anc1derived = i18n$t("opt_anc1derived")
                   ),
                   legend_labels = list(
                     facility = i18n$t("lbl_denom_facility_based"),
                     survey = i18n$t("lbl_denom_survey_national")
                   ))
            },
            i18n = i18n
          )
        },
        indicators = survey_comp_indicators
      )
    }
  )
}
