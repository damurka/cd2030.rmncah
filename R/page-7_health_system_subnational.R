hs_ratios_indicators <- c('ratio_fac_pop', 'ratio_hos_pop', 'ratio_hstaff_pop', 'ratio_bed_pop')

health_system_subnational_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_tabbed_charts_ui(ns("panel"), i18n, "title_subnational_health_system", cd_coverage_plot_ui, 
                indicators = hs_ratios_indicators, showCustom = FALSE)
  )
}

health_system_subnational_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
      nat_metrics <- reactive({
        req(cache(), active(), cache()$health_system_metrics_national)
        cache()$health_system_metrics_national
      })

      admin1_metric <- reactive({
        req(cache(), active())
        cache()$health_system_metrics_admin1 %>%
          select(adminlevel_1, year, total_pop, ratio_fac_pop, ratio_hos_pop, ratio_hstaff_pop, ratio_bed_pop, ratio_opd_pop, ratio_ipd_pop)
      })
      
      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          
          cd_coverage_plot_server(
            id = id, 
            filename = reactive(paste0(current_indicator)),
            data_fn = admin1_metric,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) {              
              plot(
                d, 
                indicator     = current_indicator,
                national_value = nat_metrics()[[current_indicator]],
                title         = i18n$t(paste0('title_metric_', current_indicator)),
                x_axis        = i18n$t(paste0('xlab_metric_', current_indicator)),
                legend_labels = list(
                  "FALSE" = i18n$t("lbl_below_benchmark"),
                  "TRUE"  = i18n$t("lbl_meets_benchmark"),
                  "threshold_line" = i18n$t("lbl_target_benchmark"), 
                  "national_line"  = i18n$t("lbl_national_average")
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = hs_ratios_indicators,
        showCustom = FALSE
      )

    }
  )
}
