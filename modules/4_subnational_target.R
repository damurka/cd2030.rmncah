subnationalTargetUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("low_reporting"),
    dashboardTitle = i18n$t("title_nav_global_coverage"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, denominatorInputUI(ns("denominator"), i18n)),
      column(6, adminLevelInputUI(ns("admin_level"), i18n, show_admin_level = FALSE))
    ),
    targetUI(ns("target"), i18n)
  )
}

subnationalTargetServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      denominatorInputServer("denominator", cache, i18n)
      admin <- adminLevelInputServer("admin_level", cache, i18n, show_admin_level = FALSE, show_district = FALSE)

      region <- reactive({
        req(admin())
        admin()$region
      })

      targetServer("target", cache, i18n, reactive("district"), region)

      countdownHeaderServer(
        "low_reporting",
        cache = cache,
        path = "7-subnational-analysis",
        i18n = i18n
      )
    }
  )
}

