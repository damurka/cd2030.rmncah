den_indicators <- c("population", "births", "under1")

denominatorAssessmentUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("denominator_assessment"),
    dashboardTitle = i18n$t("title_denom_assessment"),
    i18n = i18n,
    countdownOptions = countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, populationSelect(ns("derivation_population")))
    ),
    tabPanelsUI(ns("panel"), i18n, "title_denom_pop_comparison", downloadCoverageUI,
      indicators = den_indicators, showCustom = FALSE
    )
  )
}

denominatorAssessmentServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      populationSelectServer("derivation_population", cache)

      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          denominators <- reactive({
            req(cache())
            cache()$denominator_metrics
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_plot")),
            data_fn = denominators,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              if (current_indicator == "population") {
                my_title <- i18n$t("plt_title_denom_pop")
                my_labels <- c(i18n$t("lbl_leg_denom_un_pop"), i18n$t("lbl_leg_denom_dhis2_pop"))
              } else if (current_indicator == "births") {
                my_title <- i18n$t("plt_title_denom_births")
                my_labels <- c(i18n$t("lbl_leg_denom_un_births"), i18n$t("lbl_leg_denom_dhis2_live_births"), i18n$t("lbl_leg_denom_dhis2_tot_births"))
              } else if (current_indicator == "under1") {
                my_title <- i18n$t("plt_title_denom_under1")
                my_labels <- c(i18n$t("lbl_leg_denom_un_under1"), i18n$t("lbl_leg_denom_dhis2_under1"))
              }

              plot(d,
                metric = current_indicator,
                title = my_title,
                x_label = i18n$t("title_global_year"),
                y_label = i18n$t("opt_population"),
                legend_labels = my_labels
              )
            },
            i18n = i18n
          )
        },
        indicators = den_indicators
      )

      countdownHeaderServer(
        "denominator_assessment",
        cache = cache,
        path = "4-denominator-selection",
        section = "population-trend-comparison",
        i18n = i18n
      )
    }
  )
}
