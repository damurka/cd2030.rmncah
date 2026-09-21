subnationalInequalityUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("subnational_inequality"),
    dashboardTitle = i18n$t("title_inequ_subnational"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, denominatorInputUI(ns("denominator"), i18n)),
      column(6, adminLevelInputUI(ns("region"), i18n))
    ),
    inequalityUI(ns('subnational'), i18n = i18n)
  )
}

subnationalInequalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      denominatorInputServer("denominator", cache, i18n)
      admin <- adminLevelInputServer("region", cache, i18n, show_district = FALSE)
      
      admin_level <- reactive({
        req(admin())
        admin()$admin_level
      })

      region <- reactive({
        req(admin())
        admin()$region
      })
      
      inequalityServer("subnational", cache, i18n, admin_level, region)

      countdownHeaderServer(
        "subnational_inequality",
        cache = cache,
        path = "7-subnational-analysis",
        i18n = i18n
      )
    }
  )
}
