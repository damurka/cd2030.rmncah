sys_comparison_indicators <- c('cov_ideliv_hstaff', 'ratio_opd_u5_hstaff', 'ratio_ipd_u5_hos', 'ratio_ipd_u5_bed')
mch_comparison_indicators <- c('ratio_fac_pop', 'ratio_hstaff_pop')

health_system_comparison_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    # cd_tabbed_charts_ui(ns("panel"), i18n, "title_health_system_comparison", cd_coverage_plot_ui, 
    #             indicators = sys_comparison_indicators, showCustom = FALSE),

    cd_tabbed_charts_ui(ns("panel1"), i18n, "title_health_system_comparison", cd_coverage_plot_ui, 
                indicators = mch_comparison_indicators, showCustom = FALSE)
  )
}

health_system_comparison_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      comparison <- reactive({
        req(cache(), active(), cache()$check_inequality_params)
        cache()$health_system_comparison
      })
      
      # cd_tabbed_charts_server(
      #   "panel",
      #   serverInput = function(id, current_indicator) {
          
      #     cd_coverage_plot_server(
      #       id = id, 
      #       filename = reactive(paste0(current_indicator, "_", cache()$maternal_denominator)),
      #       data_fn = comparison,
      #       sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
      #       plot_fun = function(d) {
              
      #         # Optional parameter handling based on cov vs ratio
      #         denom <- if (str_detect(current_indicator, 'cov_')) cache()$maternal_denominator else NULL
              
      #         plot(
      #           d, 
      #           indicator   = current_indicator, 
      #           denominator = denom,
      #           title       = i18n$t(paste0("title_", current_indicator)),
      #           x_axis      = i18n$t(paste0("xlab_", current_indicator)),
      #           y_axis      = i18n$t(paste0("ylab_", current_indicator)),
      #           caption     = if (!is.null(denom)) paste(i18n$t("lbl_denom_derived"), denom) else NULL,
      #           legend_labels = list(
      #             admin  = i18n$t("lbl_admin1_units"),
      #             linear = i18n$t("lbl_linear_fit")
      #           )
      #         )
      #       },
      #       i18n = i18n
      #     )
      #   },
      #   indicators = sys_comparison_indicators
      # )

      cd_tabbed_charts_server(
        "panel1",
        serverInput = function(id, current_indicator) {

          mch_curative_comparison <- reactive({
            req(cache())
            cache()$generate_phc_scatter_data(indicator = current_indicator)
          })
          
          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = mch_curative_comparison,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) {
              plot(
                d, 
                title  = i18n$t(paste0("title_scatter_", current_indicator)),
                x_axis = i18n$t(paste0("xlab_", current_indicator)),
                y_axis = i18n$t("ylab_phc_scatter"),
                quad_labels = list(
                  high_high = i18n$t(paste0("lbl_hh_", current_indicator)),
                  low_high  = i18n$t(paste0("lbl_lh_", current_indicator)),
                  high_low  = i18n$t(paste0("lbl_hl_", current_indicator)),
                  low_low   = i18n$t(paste0("lbl_ll_", current_indicator))
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = mch_comparison_indicators,
        showCustom = FALSE
      )

    }
  )
}
