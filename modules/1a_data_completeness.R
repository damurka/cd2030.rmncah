dataCompletenessUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("data_completeness"),
    dashboardTitle = i18n$t("title_complete_main"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, indicatorSelect(ns("indicator"), i18n, tooltip = "tt_complete_indicator_missing", select_all = TRUE)),
      column(6, adminLevelInputUI(ns("admin_level"), i18n))
    ),
    tabBox(
      title = tags$span(icon("chart-line"), i18n$t("title_complete_missing_indicators")),
      width = 12,
      tabPanel(
        title = i18n$t("opt_heat_map"),
        plotDownloadsRowUI(ns("completeness_heatmap"))
      ),
      tabPanel(
        title = i18n$t("title_complete_indicators"),
        plotDownloadsRowUI(ns("completeness_district"))
      ),
      tabPanel(
        title = i18n$t("title_complete_missing_region"),
        plotDownloadsRowUI(ns("completeness_linegraph"))
      )
    ),
    tableDownloadsUI(ns("incomplete_district"), i18n, "title_complete_district_missing")
  )
}

dataCompletenessServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      indicator <- indicatorSelectServer("indicator")
      admin <- adminLevelInputServer("admin_level", cache, i18n, allow_select_all = TRUE, show_district = FALSE)

      admin_level <- reactive({
        req(admin())
        admin()$admin_level
      })

      region <- reactive({
        req(admin())
        admin()$region
      })

      completeness_districts <- reactive({
        req(cache())
        if (!is.null(region())) {
          cache()$district_completeness
        } else {
          cache()$calculate_district_completeness_summary(region())
        }
      })

      completeness_summary <- reactive({
        req(cache(), admin_level())
        if (admin_level() == "adminlevel_1" && is.null(region())) {
          cache()$completeness_admin1
        } else if (admin_level() == "district" && is.null(region())) {
          cache()$completeness_district
        } else {
          cache()$calculate_completeness_summary(admin_level(), region())
        }
      })

      incomplete_district <- reactive({
        req(cache(), indicator())

        cache()$list_missing_units(indicator(), region())
      })
      
      tableDownloadsServer(
        "incomplete_district",
        cache,
        i18n,
        control_type = "year",
        data = incomplete_district,
        filename = reactive(paste0("checks_incomplete_districts_", indicator())),
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("title_complete_district_missing_1")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t("title_complete_district_missing_ind")), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = data, startCol = 1, startRow = 3)
        }
      )

      plotDownloadsRowServer(
        id = "completeness_heatmap",
        i18n = i18n,
        plot_data = completeness_summary,
        plot_filename = reactive("completeness_heatmap"),
        plot_fun = function(d) {
          indicator <- if (!is.null(indicator()) && nzchar(indicator())) i18n$t(paste0("opt_", indicator())) else NULL
          region <- region()
          admin_level <- i18n$t(if (is.null(region)) paste0("opt_", admin_level()) else "opt_district")

          title <- if (nzchar(indicator())) {
            if (is.null(region())) {
              "plt_title_complete_heatmap_one"
            } else {
              "plt_title_complete_heatmap_one_reg"
            }
          } else {
            if (is.null(region())) {
              "plt_title_complete_heatmap_all"
            } else {
              "plt_title_complete_heatmap_all_reg"
            }
          }
          plot(
            d,
            indicator = indicator(),
            plot_type = "heat_map",
            title = str_glue(i18n$t(title)),
            x_axis = admin_level,
            y_axis = i18n$t(if (!is.null(indicator)) "title_global_year" else "title_global_indicator"),
            legend = i18n$t("lbl_leg_complete_heatmap")
          )
        },
        excel_write_fun = function(wb, d) {
          req(cache())
          completeness_rate <- cache()$completeness_national
          district_completeness_rate <- cache()$district_completeness

          sheet_name_1 <- i18n$t("title_complete_missing")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("tab_complete_monthly"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          sheet_name_2 <- i18n$t("lbl_sheet_complete_district")
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = i18n$t("tab_complete_district"), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)
        }
      )

      plotDownloadsRowServer(
        id = "completeness_district",
        i18n = i18n,
        plot_data = completeness_districts,
        plot_filename = reactive("completeness_district"),
        plot_fun = function(d) {
          req(indicator())
          indicator <- if (!is.null(indicator()) || nzchar(indicator())) i18n$t(paste0("opt_", indicator())) else NULL
          plot(
            d,
            indicator = indicator(),
            title = str_glue(i18n$t("plt_title_complete_district_plot")),
            y_axis = i18n$t("lbl_axis_y_complete_trend"),
            x_axis = i18n$t("title_global_year")
          )
        }
      )

      plotDownloadsRowServer(
        id = "completeness_linegraph",
        i18n = i18n,
        plot_data = completeness_summary,
        plot_filename = reactive("completeness_linegraph"),
        plot_fun = function(d) {
          req(indicator())
          indicator <- if (!is.null(indicator()) || nzchar(indicator())) i18n$t(paste0("opt_", indicator())) else NULL
          admin_level <- i18n$t(if (is.null(region())) paste0("opt_", admin_level()) else "opt_district")
          plot(
            d,
            indicator = indicator(),
            plot_type = "trend",
            title = str_glue(i18n$t("plt_title_complete_trend")),
            y_axis = i18n$t("lbl_axis_y_complete_trend"),
            x_axis = i18n$t("title_global_year")
          )
        }
      )

      countdownHeaderServer(
        "data_completeness",
        cache = cache,
        path = "2-data-quality-assessment",
        section = "data-missingness",
        i18n = i18n
      )
    }
  )
}
