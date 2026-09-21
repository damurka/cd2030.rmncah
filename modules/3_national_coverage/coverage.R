nat_cov_indicators <- c('anc4', "instlivebirths", "low_bweight", 'penta3', "measles1", 'fpet')

coverageUI <- function(id, i18n, key_label) {
  ns <- NS(id)
  tabPanelsUI(ns("panel"), i18n, key_label, downloadCoverageUI, indicators = nat_cov_indicators)
}

coverageServer <- function(id, cache, i18n, admin_level, region = reactive(NULL)) {
  stopifnot(is.reactive(cache))
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          denom_rx <- reactive({
            req(cache())
            cache()$get_denominator(current_indicator)
          })
          data_rx <- reactive({
            req(cache(), cache()$check_coverage_params)
            cache()$get_filtered_coverage(indicator = current_indicator, admin_level = admin_level(), region = region())
          })
          fpet_data <- reactive({
            req(cache())
            cache()$fpet_data %>% 
              filter(year >= 2010)
          })
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_", region(), "_survey_", denom_rx())),
            data_fn = if (current_indicator == 'fpet') fpet_data else data_rx,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {

              if (current_indicator != 'fpet') {
                indicator <- i18n$t(paste0("opt_", current_indicator))
                denominator <- i18n$t(paste0("opt_", denom_rx()))
                region <- region()
                title <- if (admin_level() == "national") "plt_title_coverage_nat_est" else "plt_title_coverage_subnat_est"
                plot(d,
                    title = str_glue(i18n$t(title)),
                    x_axis = i18n$t("title_global_year"),
                    y_axis = str_glue(i18n$t("lbl_axis_y_coverage")),
                    caption = str_glue(i18n$t("plt_caption_coverage_denom")),
                    labels = list(
                      dhis2  = i18n$t("lbl_coverage_dhis2_est"),
                      wuenic = i18n$t("lbl_coverage_wuenic_est"),
                      survey = i18n$t("lbl_coverage_survey_est"),
                      ci     = i18n$t("lbl_coverage_95ci")
                    ))
                } else {
                  # Extract country to append to the translated base title
                  country_name <- attr(d, "country") %||% ""
                  base_title <- i18n$t("title_graph_fpet")
                  
                  plot(
                    d, 
                    title = paste0(base_title, ", ", country_name),
                    x_axis = i18n$t("title_global_year"),
                    y_axis = i18n$t("ylab_fpet"),
                    caption = i18n$t("caption_fpet"),
                    
                    # Translate the legend (adjust the keys based on what they are named in your CSV/tibble)
                    indicator_labels = list(
                      prevalence = i18n$t("lbl_fpet_mcpr"),
                      demand = i18n$t("lbl_fpet_demand")
                    )
                  )
                }
              },
            i18n = i18n
          )
        },
        indicators = nat_cov_indicators
      )
    }
  )
}
