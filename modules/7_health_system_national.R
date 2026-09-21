healthSystemNationalUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('health_sys_national'),
    dashboardTitle = i18n$t('title_national_health_system'),
    i18n = i18n,

    box(
      title = i18n$t('opt_health_system_density'),
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

healthSystemNationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_metrics <- reactive({
        req(cache())
        # cache()$generate_health_system_table()
        cache()$generate_health_system_table(
          labels = list(
            section = list(
              infrastructure = i18n$t("sec_infrastructure"),
              workforce      = i18n$t("sec_workforce"),
              private_sector = i18n$t("sec_private_sector")
            ),
            indicator = list(
              fac_density   = i18n$t("ind_fac_density"),
              hosp_share    = i18n$t("ind_hosp_share"),
              hosp_density  = i18n$t("ind_hosp_density"),
              bed_density   = i18n$t("ind_bed_density"),
              hwf_density   = i18n$t("ind_hwf_density"),
              skill_mix     = i18n$t("ind_skill_mix"),
              private_share = i18n$t("ind_private_share"),
              ngo_share     = i18n$t("ind_ngo_share")
            ),
            unit = list(
              per_10k  = i18n$t("unit_per_10k"),
              per_100k = i18n$t("unit_per_100k"),
              pct      = "%"
            )
          )
        )
      })

      output$overall_score <- renderUI({
        req(national_metrics())
        latest_year <- max(cache()$data_years)
        out <- national_metrics() %>%
          plot(
            year            = latest_year, 
            indicator_label = i18n$t("title_global_indicator"),
            value_label     = i18n$t("lbl_value"),
            unit_label      = i18n$t("lbl_unit")
          ) %>% 
          # plot(years = cache()$data_years, title = i18n$t("lbl_score_metric_header")) %>%
          htmltools_value()
        HTML(as.character(out))
      })

      downloadButtonServer(
        id = "download_data",
        filename = reactive("overall_score"),
        extension = reactive("xlsx"),
        data = national_metrics,
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
        data = national_metrics,
        label = "btn_global_download_plot",
        button_class = "btn-plot"
      )

      countdownHeaderServer(
        'health_sys_national',
        cache = cache,
        path = '11-health-system-performance',
        i18n = i18n
      )
    }
  )
}
