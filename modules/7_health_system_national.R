health_system_national_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_chart_card(
      title = i18n$t('opt_health_system_density'),
      chart_toolbar = tagList(cd_download_button_ui(ns("download_plot")), cd_download_button_ui(ns("download_data"))),
      i18n = i18n,
      width = 12,
      cd_spinner(uiOutput(ns("overall_score")))
    )
  )
}

health_system_national_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
      national_metrics <- reactive({
        req(cache(), active())
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

      cd_download_button_server(
        id = "download_data",
        filename = reactive("overall_score"),
        extension = reactive("xlsx"),
        data = national_metrics,
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
        button_class = "cd-tool-btn"
      )

    }
  )
}
