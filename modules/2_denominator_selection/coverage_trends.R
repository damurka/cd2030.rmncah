cov_trend_indicators <- c("instlivebirths", "penta3")

coverageTrendsUI <- function(id, i18n) {
  ns <- NS(id)
  tabPanelsUI(
    ns("panel"), 
    i18n, 
    "title_denom_pop_trend",
    uiInput = downloadCoverageUI,
    indicators = cov_trend_indicators
  )
}

coverageTrendsServer <- function(id, cache, admin_level, region, i18n) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(admin_level))
  stopifnot(is.reactive(region))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          coverage <- reactive({
            req(cache(), cache()$check_inequality_params, admin_level())
            cache()$calculate_derived_coverage(current_indicator, admin_level(), region())
          })

          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0("sheet_", current_indicator, "_derived_coverage")),
            data_fn = coverage,
            sheet_name = reactive(i18n$t(paste0(current_indicator, "_derived_coverage"))),
            plot_fun = function(d) {
              req(admin_level())
              if (admin_level() != "national") req(region())
              region_name <- region()
              title <- if (is.null(region())) "plt_title_denom_nat_trend" else "plt_title_denom_subnat_trend"
              indicator <- i18n$t(paste0("opt_", current_indicator))
              plot(d, 
                   type = 'trend',
                   region = region(),
                   title = str_glue(i18n$t(title)),
                   x_label = i18n$t("title_global_year"),
                   y_label = str_glue(i18n$t("lbl_axis_y_coverage")),
                   legend_labels = c(
                     "un"            = i18n$t("opt_un"),
                     "dhis2"         = i18n$t("opt_dhis2"),
                     "anc1"          = i18n$t("opt_anc1"),
                     "penta1"        = i18n$t("opt_penta1"),
                     "penta1derived" = i18n$t("opt_penta1derived"),
                     "anc1derived" = i18n$t("opt_anc1derived")
                   ))
            },
            i18n = i18n
          )
        },
        indicators = cov_trend_indicators
      )

      countdownHeaderServer(
        "derived_coverage",
        cache = cache,
        path = "denominator-assessment",
        section = "sec-derived-coverage",
        i18n = i18n
      )
    }
  )
}
