subnationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("subnational_coverage"),
    dashboardTitle = i18n$t("title_nav_subnational_coverage"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, denominatorInputUI(ns("denominator"), i18n)),
      column(6, adminLevelInputUI(ns("admin_level"), i18n))
    ),
    coverageUI(ns("coverage"), i18n, "title_nav_subnational_coverage")
  )
}

subnationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      denominatorInputServer("denominator", cache, i18n)
      admin <- adminLevelInputServer("admin_level", cache, i18n)

      admin_level <- reactive({
        req(admin())
        admin()$admin_level
      })

      region <- reactive({
        req(admin())
        admin()$region
      })

      coverageServer("coverage", cache, i18n, admin_level, region)

      countdownHeaderServer(
        "subnational_coverage",
        cache = cache,
        path = "7-subnational-analysis",
        i18n = i18n
      )
    }
  )
}
