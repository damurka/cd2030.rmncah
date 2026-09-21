hs_ratios_indicators <- c('ratio_fac_pop', 'ratio_hos_pop', 'ratio_hstaff_pop', 'ratio_bed_pop')

healthSystemSubnationalUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('health_sys_national'),
    dashboardTitle = i18n$t('title_subnational_health_system'),
    i18n = i18n,
    
    tabPanelsUI(ns("panel"), i18n, "title_subnational_health_system", downloadCoverageUI, 
                indicators = hs_ratios_indicators, showCustom = FALSE)
  )
}

healthSystemSubnationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nat_metrics <- reactive({
        req(cache(), cache()$health_system_metrics_national)
        cache()$health_system_metrics_national
      })

      admin1_metric <- reactive({
        req(cache())
        cache()$health_system_metrics_admin1 %>%
          select(adminlevel_1, year, total_pop, ratio_fac_pop, ratio_hos_pop, ratio_hstaff_pop, ratio_bed_pop, ratio_opd_pop, ratio_ipd_pop)
      })
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          downloadCoverageServer(
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
        indicators = hs_ratios_indicators
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
