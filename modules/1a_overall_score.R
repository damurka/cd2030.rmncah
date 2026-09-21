overallScoreUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("data_quality"),
    dashboardTitle = i18n$t("title_score_main"),
    i18n = i18n,
    
    include_report = TRUE,
    
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, adminLevelInputUI(ns("region"), i18n, show_admin_level = FALSE))
    ),
    box(
      title = i18n$t("title_score_main"),
      status = "success",
      width = 12,
      div(
        class = "cd-plot-wrap",
        withSpinner(uiOutput(ns("overall_score"))),
        div(
          class = "cd-toolbox",
          downloadButtonUI(ns("download_plot")),
          downloadButtonUI(ns("download_data"))
        )
      )
    )
  )
}

overallScoreServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      admin <- adminLevelInputServer("region", cache, i18n, allow_select_all = TRUE, show_district = FALSE, show_admin_level = FALSE)
      region <- reactive({
        req(admin())
        admin()$region
      })

      overall_score <- reactive({
        req(cache())

        translated_labels <- list(
          header = list(
            h1 = i18n$t("title_score_monthly_complete"),
            h2 = i18n$t("title_score_extreme_outliers"),
            h3 = i18n$t("title_score_consist_annual")
          ),
          section = list(
            r1a   = i18n$t("lbl_score_1a"),
            # Combine text + threshold number
            r1b   = paste0(i18n$t("lbl_score_1b_prefix"), cache()$performance_threshold),
            r1c   = if (get_selected_group() == "vaccine") i18n$t("lbl_score_1c_vaccine") else i18n$t("lbl_score_1c_rmncah"),
            r2a   = i18n$t("lbl_score_2a"),
            r2b   = i18n$t("lbl_score_2b"),
            score = i18n$t("lbl_score_annual_score")
          ),
          metric = list(
            r_anc1_penta1    = i18n$t("lbl_consist_ratio_anc1_penta1"),
            r_penta1_penta3  = i18n$t("lbl_consist_ratio_penta1_penta3"),
            ok_anc1_penta1   = i18n$t("lbl_score_range_anc1_penta1"),
            ok_penta1_penta3 = i18n$t("lbl_score_range_penta1_penta3")
          )
        )
        if (is.null(region())) {
          cache()$calculate_overall_score("national", labels = translated_labels)
        } else {
          cache()$calculate_overall_score("adminlevel_1", region(), labels = translated_labels)
        }
      })

      output$overall_score <- renderUI({
        req(overall_score())
        out <- overall_score() %>%
          plot(years = cache()$data_years, title = i18n$t("lbl_score_metric_header")) %>%
          htmltools_value()
        HTML(as.character(out))
      })

      downloadButtonServer(
        id = "download_data",
        filename = reactive("overall_score"),
        extension = reactive("xlsx"),
        data = overall_score,
        i18n = i18n,
        label = "btn_global_download_data",
        icon = "table",
        button_class = "btn-plot",
        content = function(file, d) {
          wb <- createWorkbook()
          sheet_name_2 <- i18n$t("lbl_score_metric_header")
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = d, startCol = 1, startRow = 1)
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )

      downloadButtonServer(
        id = "download_plot",
        filename = reactive("overall_score"),
        extension = reactive("png"),
        i18n = i18n,
        icon = "camera",
        content = function(file, plot_data) {
          out <- plot_data %>%
            plot(years = cache()$data_years, title = i18n$t("lbl_score_metric_header")) %>%
            save_as_image(path = file, zoom = 3)
        },
        data = overall_score,
        label = "btn_global_download_plot",
        button_class = "btn-plot"
      )

      countdownHeaderServer(
        "data_quality",
        cache = cache,
        path = "2-data-quality-assessment",
        section = "overall-quality-score",
        i18n = i18n
      )
    }
  )
}
