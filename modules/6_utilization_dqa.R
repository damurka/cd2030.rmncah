utilizationDqaUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("data_quality"),
    dashboardTitle = i18n$t("title_utilization_dqa"),
    i18n = i18n,
    
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, adminLevelInputUI(ns("region"), i18n, show_admin_level = FALSE))
    ),
    box(
      title = i18n$t("title_utilization_dqa"),
      status = "success",
      width = 12,
      div(
        class = "cd-plot-wrap",
        withSpinner(uiOutput(ns("utilization_dqa"))),
        div(
          class = "cd-toolbox",
          downloadButtonUI(ns("download_plot")),
          downloadButtonUI(ns("download_data"))
        )
      )
    )
  )
}

utilizationDqaServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      admin <- adminLevelInputServer("region", cache, i18n, allow_select_all = TRUE, show_district = FALSE, show_admin_level = FALSE)
      
      region <- reactive({
        req(admin())
        admin()$region
      })

      utilization_dqa <- reactive({
        req(cache())

        # UPDATED: Match the new 'header' and 'indicator' structure from the refactored function
        translated_labels <- list(
          header = list(
            h1 = i18n$t("title_score_monthly_complete"),
            h2 = i18n$t("title_score_extreme_outliers"),
            h3 = i18n$t("title_score_service_dqa") # Ensure you have a translation key for this
          ),
          indicator = list(
            opd_rr                   = i18n$t("lbl_opd_rr"), 
            district_opd_rr          = paste0(i18n$t("lbl_score_1b_prefix"), cache()$performance_threshold),
            mis_opd_under5           = i18n$t("lbl_mis_opd_under5"),
            mis_ipd_under5           = i18n$t("lbl_mis_ipd_under5"),
            districts_no_missing_opd = i18n$t("lbl_districts_no_missing_opd"),
            districts_no_missing_ipd = i18n$t("lbl_districts_no_missing_ipd"),
            
            opd_under5_outlier5std   = i18n$t("lbl_opd_under5_outlier5std"),
            district_no_outlier_opd  = i18n$t("lbl_district_no_outlier_opd"),
            
            ratio_opd_u5_ipd_u5      = i18n$t("lbl_ratio_opd_u5_ipd_u5"),
            perc_opd_under5          = i18n$t("lbl_perc_opd_under5"),
            perc_ipd_under5          = i18n$t("lbl_perc_ipd_under5")
          )
        )
        
        if (is.null(region())) {
          cache()$calculate_service_dqa_summary("national", labels = translated_labels)
        } else {
          cache()$calculate_service_dqa_summary("adminlevel_1", region(), labels = translated_labels)
        }
      })

      output$utilization_dqa <- renderUI({
        req(utilization_dqa())
        out <- utilization_dqa() %>%
          plot(years = cache()$data_years, title = i18n$t("lbl_score_metric_header")) %>%
          htmltools_value()
        HTML(as.character(out))
      })

      downloadButtonServer(
        id = "download_data",
        filename = reactive("utilization_dqa"),
        extension = reactive("xlsx"),
        data = utilization_dqa,
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
        filename = reactive("utilization_dqa"),
        extension = reactive("png"),
        i18n = i18n,
        icon = "camera",
        content = function(file, plot_data) {
          out <- plot_data %>%
            plot(years = cache()$data_years, title = i18n$t("lbl_score_metric_header")) %>%
            save_as_image(path = file, zoom = 3)
        },
        data = utilization_dqa,
        label = "btn_global_download_plot",
        button_class = "btn-plot"
      )

      countdownHeaderServer(
        "data_quality",
        cache = cache,
        path = "10-service-utilisation",
        # section = "sec-dqa-overall-score", # You might want to update this ID if the section changed
        i18n = i18n
      )
    }
  )
}
