source("modules/3_national_coverage/coverage.R")

nationalCoverageUI <- function(id, i18n, label) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("national_coverage"),
    dashboardTitle = i18n$t("title_coverage_national"),
    i18n = i18n,
    coverageUI(ns("coverage"), i18n, "title_coverage_national")
  )
}

nationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      coverageServer("coverage", cache, i18n, reactive("national"))
      
      countdownHeaderServer(
        "national_coverage",
        cache = cache,
        path = "5-coverage-estimation",
        i18n = i18n
      )
    }
  )
}
