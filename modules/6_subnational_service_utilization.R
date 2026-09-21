source("modules/6_national_service_utilization/service_utilization.R")
source("modules/6_national_service_utilization/service_utilization_capita.R")

subnationalServiceUtilizationUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('service_utilization'),
    dashboardTitle = i18n$t("title_subnational_utilization"),
    i18n = i18n,

    countdownOptions = countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, adminLevelInputUI(ns('admin'), i18n, show_admin_level = FALSE))
    ),
    
    utilizationUI(ns('subnational'), i18n, 'title_subnational_utilization'),
    utilizationCapitaUI(ns('subnational_capita'), i18n, 'title_subnational_utilization')
  )
}

subnationalServiceUtilizationServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      admin <- adminLevelInputServer("admin", cache, i18n, show_admin_level = FALSE, show_district = FALSE)

      region <- region <- reactive({
        req(admin())
        admin()$region
      })
      
      utilizationServer('subnational', cache, i18n, 'adminlevel_1', region)
      utilizationCapitaServer('subnational_capita', cache, i18n)

      countdownHeaderServer(
        'service_utilization',
        cache = cache,
        path = '10-service-utilisation',
        i18n = i18n
      )
    }
  )
}
