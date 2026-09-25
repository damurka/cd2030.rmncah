
subnational_service_utilization_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    filters = cd_filter_bar(
      cd_admin_level_ui(ns('admin'), i18n, show_admin_level = FALSE)
    ),
    
    utilization_ui(ns('subnational'), i18n, 'title_subnational_utilization'),
    utilization_capita_ui(ns('subnational_capita'), i18n, 'title_subnational_utilization')
  )
}

subnational_service_utilization_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      admin <- cd_admin_level_server("admin", cache, i18n, show_admin_level = FALSE, show_district = FALSE)

      region <- region <- reactive({
        req(admin())
        admin()$region
      })

      utilization_server('subnational', cache, i18n, 'adminlevel_1', region, active = active)
      utilization_capita_server('subnational_capita', cache, i18n, active = active)

    }
  )
}
