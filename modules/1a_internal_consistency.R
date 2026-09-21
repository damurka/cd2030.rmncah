source("modules/1a_internal_consistency/ratios.R")
source("modules/1a_internal_consistency/consistency_check.R")

internalConsistencyUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("internal_consistency"),
    dashboardTitle = i18n$t("title_consist_main"),
    i18n = i18n,
    calculateRatiosUI(ns("ratios"), i18n = i18n),
    consistencyCheckUI(ns("consistency"), i18n = i18n)
  )
}

internalConsistencyServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      calculateRatiosServer("ratios", cache, i18n)
      consistencyCheckServer("consistency", cache, i18n)

      countdownHeaderServer(
        "internal_consistency",
        cache = cache,
        path = "2-data-quality-assessment",
        section = "ratio-calculations",
        i18n = i18n
      )
    }
  )
}
