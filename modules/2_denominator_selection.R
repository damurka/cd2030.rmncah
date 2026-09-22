source("modules/2_denominator_selection/coverage_trends.R")
source("modules/2_denominator_selection/survey_comparison.R")
source("modules/2_denominator_selection/subnational_denominator.R")

denominatorSelectionUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("denominator_selection"),
    dashboardTitle = i18n$t("title_denom_selection"),
    i18n = i18n,
    countdownOptions = countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, denominatorInputUI(ns("maternal_denominator"), i18n, allow_input = TRUE, is_maternal = TRUE)),
      column(3, denominatorInputUI(ns("vaxx_denominator"), i18n, allow_input = TRUE)),
      column(6, adminLevelInputUI(ns("admin_level"), i18n, include_national = TRUE))
    ),
    include_report = TRUE,
    surveyComparisonUI(ns("survey"), i18n),
    subnationalDenominatorUI(ns('subnational'), i18n),
    coverageTrendsUI(ns("coverage"), i18n)
  )
}

denominatorSelectionServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      denominatorInputServer("maternal_denominator", cache, i18n, allowInput = TRUE, is_maternal = TRUE)
      denominatorInputServer("vaxx_denominator", cache, i18n, allowInput = TRUE)
      admin <- adminLevelInputServer("admin_level", cache, i18n)

      admin_level <- reactive({
        req(admin())
        admin()$admin_level
      })

      region <- reactive({
        req(admin())
        admin()$region
      })

      coverageTrendsServer("coverage", cache, admin_level, region, i18n)
      surveyComparisonServer("survey", cache, admin_level, region, i18n)
      subnationalDenominatorServer('subnational', cache, i18n)

      countdownHeaderServer(
        "denominator_selection",
        cache = cache,
        path = "4-denominator-selection",
        # section = "population-trend-comparison",
        i18n = i18n
      )
    }
  )
}
