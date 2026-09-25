utilization_dqa_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    filters = cd_filter_bar(
      cd_admin_level_ui(ns("region"), i18n, show_admin_level = FALSE)
    ),
    cd_chart_card(
      title = i18n$t("title_utilization_dqa"),
      chart_toolbar = tagList(cd_download_button_ui(ns("download_plot")), cd_download_button_ui(ns("download_data"))),
      i18n = i18n,
      status = "success",
      width = 12,
      cd_spinner(uiOutput(ns("utilization_dqa")))
    )
  )
}

utilization_dqa_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      admin <- cd_admin_level_server("region", cache, i18n, allow_select_all = TRUE, show_district = FALSE, show_admin_level = FALSE)

      region <- reactive({
        req(admin())
        admin()$region
      })

      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
      utilization_dqa <- reactive({
        req(cache(), active())

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

      cd_download_button_server(
        id = "download_data",
        filename = reactive("utilization_dqa"),
        extension = reactive("xlsx"),
        data = utilization_dqa,
        i18n = i18n,
        label = "btn_global_download_data",
        icon = "table",
        button_class = "cd-tool-btn",
        content = function(file, d) {
          wb <- createWorkbook()
          cd_add_sheet(wb, i18n$t("lbl_score_metric_header"), d)
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )

      cd_download_button_server(
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
        button_class = "cd-tool-btn"
      )

    }
  )
}
