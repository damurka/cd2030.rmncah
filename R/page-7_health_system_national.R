health_system_national_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_table_card_ui(ns("national_metrics"), i18n, i18n$t('opt_health_system_density'))
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

      # the table of the latest year, on screen and in its picture
      cd_table_card_server(
        "national_metrics",
        i18n,
        data = national_metrics,
        table_fun = function(d) {
          plot(
            d,
            year            = max(cache()$data_years),
            indicator_label = i18n$t("title_global_indicator"),
            value_label     = i18n$t("lbl_value"),
            unit_label      = i18n$t("lbl_unit")
          )
        },
        filename = reactive("health_system_national")
      )
    }
  )
}
