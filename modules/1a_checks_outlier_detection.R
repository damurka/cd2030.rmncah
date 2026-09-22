outlierDetectionUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("outlier_detection"),
    dashboardTitle = i18n$t("title_outlier_main"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, indicatorSelect(ns("indicator"), i18n, tooltip = "tt_outlier_indicator")),
      column(6, adminLevelInputUI(ns("admin_level"), i18n))
    ),
    tabBox(
      title = tags$span(icon("chart-line"), i18n$t("title_outlier_indicators")),
      width = 12,
      tabPanel(
        title = i18n$t("opt_heat_map"),
        plotDownloadsRowUI(ns("outlier_heatmap"))
      ),
      tabPanel(
        title = i18n$t("title_outlier_indicator_bar"),
        plotDownloadsRowUI(ns("outlier_bargraph"))
      ),
      tabPanel(
        title = i18n$t("title_outlier_region_bar"),
        plotDownloadsRowUI(ns("outlier_region_bargraph"))
      )
    ),
    tableDownloadsUI(ns("district_outlier_summary"), i18n, "title_outlier_district"),
    box(
      title = i18n$t("title_outlier_district_trends"),
      status = "success",
      collapsible = TRUE,
      width = 6,
      fluidRow(
        column(6, adminLevelInputUI(ns("region_trend"), i18n, show_admin_level = FALSE)),
        column(12, plotDownloadsRowUI(ns("outlier_district_trend")))
      )
    )
  )
}

outlierDetectionServer <- function(id, cache, i18n) {
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

      admin_level_trend <- reactive({
        req(admin_level())
        get_plot_admin_column(admin_level(), region())
      })

      region_admin <- adminLevelInputServer("region_trend", cache, i18n, show_admin_level = FALSE, selected_admin1 = region)
      region_trend <- reactive({
        ra <- region_admin()
        if (is.null(ra)) {
          return(NULL)
        }
        ra$region
      })

      outlier_summary <- reactive({
        req(cache(), admin_level())
        if (admin_level() == "adminlevel_1" && is.null(region())) {
          cache()$outliers_admin1
        } else if (admin_level() == "district" && is.null(region())) {
          cache()$outliers_district
        } else {
          cache()$calculate_outliers_summary(admin_level(), region())
        }
      })

      outlier_districts <- reactive({
        req(cache())
        cache()$list_outlier_units
      })

      plotDownloadsRowServer(
        id = "outlier_heatmap",
        i18n = i18n,
        plot_data = outlier_summary,
        plot_filename = reactive("outlier_heatmap"),
        plot_fun = function(d) {
          req(indicator())
          indicator <- i18n$t(paste0("opt_", indicator()))
          admin_level <- i18n$t(if (is.null(region()) || !nzchar(region())) paste0("opt_", admin_level()) else "opt_district")
          title <- if (is.null(region()) || !nzchar(region())) "plt_title_outlier_heatmap_all" else "plt_title_outlier_heatmap_one"
          plot(
            d,
            "heat_map",
            indicator(),
            title = str_glue(i18n$t(title)),
            x_axis = admin_level,
            y_axis = i18n$t(if (nzchar(admin_level())) "lbl_leg_outlier" else "title_global_indicator"),
            legend = i18n$t("lbl_leg_outlier")
          )
        },
        excel_write_fun = function(wb, d) {
          req(cache())

          sheet_name_1 <- i18n$t("title_outlier_extreme")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("tab_outlier_extreme"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = cache()$outliers_national, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- i18n$t("lbl_sheet_outlier_district")
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = i18n$t("tab_outlier_district"), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = cache()$district_outliers_summary, startCol = 1, startRow = 3)
        }
      )

      plotDownloadsRowServer(
        id = "outlier_bargraph",
        i18n = i18n,
        plot_data = outlier_summary,
        plot_filename = reactive("outlier_bargraph"),
        plot_fun = function(d) {
          req(indicator())
          plot(
            d,
            "indicator",
            indicator(),
            title = i18n$t("plt_title_outlier_indicator"),
            x_axis = i18n$t("title_global_year"),
            y_axis = i18n$t("lbl_leg_outlier"),
            legend = i18n$t("lbl_leg_outlier")
          )
        }
      )

      plotDownloadsRowServer(
        id = "outlier_region_bargraph",
        i18n = i18n,
        plot_data = outlier_summary,
        plot_filename = reactive("outlier_region_bargraph"),
        plot_fun = function(d) {
          req(indicator())
          admin_level <- i18n$t(if (is.null(region()) || !nzchar(region())) paste0("opt_", admin_level()) else "opt_district")
          plot(
            d,
            region = region(),
            indicator = indicator(),
            title = str_glue(i18n$t("plt_title_outlier_region")),
            x_axis = i18n$t("title_global_year"),
            y_axis = i18n$t("lbl_leg_outlier"),
            legend = i18n$t("lbl_leg_outlier")
          )
        }
      )
      
      selected_year <- tableDownloadsServer(
        "district_outlier_summary",
        cache,
        i18n,
        control_type = "year",
        data = outlier_districts,
        data_transform = function(d) {
          outlier_col <- paste0(indicator(), "_outlier5std")
          d %>%
            select(any_of(c("year", "month", "adminlevel_1", "district", indicator(), paste0(indicator(), c("_med", "_mad")), outlier_col))) %>%
            filter(!!sym(outlier_col) == 1) %>%
            select(-!!sym(outlier_col))
        },
        filename = reactive("district_low_reporting_rate")
      )

      plotDownloadsRowServer(
        id = "outlier_district_trend",
        i18n = i18n,
        plot_data = outlier_districts,
        plot_filename = reactive(paste0("outlier_district_trend", indicator(), "_",selected_year())),
        plot_fun = function(d) {
          req(indicator(), region_trend(), selected_year())
          indicator_name <- i18n$t(paste0("opt_", indicator()))
          year_val <- selected_year()
          region_name <- region_trend()
          plot(
            d,
            indicator(),
            region = region_trend(),
            year = as.integer(year_val),
            title = str_glue(i18n$t("plt_title_outlier_trend_year")),
            x_axis = i18n$t("lbl_axis_x_outlier_trend"),
            y_axis = indicator_name,
            legend = i18n$t("lbl_leg_outlier_trend"),
            label = c(
              reported = i18n$t("lbl_outlier_series_reported"),
              median   = i18n$t("lbl_outlier_series_median"),
              bounds   = i18n$t("lbl_outlier_series_bounds"),
              outliers = i18n$t("lbl_outlier_series_outliers")
            )
          )
        },
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("title_outlier_district_extreme")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t("title_outlier_district_extreme_year")), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = d, startCol = 1, startRow = 3)
        }
      )

      countdownHeaderServer(
        "outlier_detection",
        cache = cache,
        path = "2-data-quality-assessment",
        section = "outlier-detection",
        i18n = i18n
      )
    }
  )
}
