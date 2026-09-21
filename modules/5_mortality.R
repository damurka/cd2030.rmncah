mort_indicators <- c('mmr_inst', 'sbr_inst', 'nn_inst')
subnational_mort_indicators <- c("fresh_total_sb", "ratio_md_sb", "ratio_md_nd")

mortalityUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_institutional'),
    i18n = i18n,
    tabPanelsUI(ns("panel"), i18n, "title_mortality_institutional", downloadCoverageUI, 
                indicators = mort_indicators, showCustom = FALSE),
    tabPanelsUI(ns("panel1"), i18n, "title_subnational_mortality_institutional", downloadCoverageUI, 
                indicators = subnational_mort_indicators, showCustom = FALSE)
  )
}

mortalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = reactive(cache()$mortality_summary),
            sheet_name = reactive(current_indicator),
            plot_fun = function(d) plot(d, indicator = current_indicator),
            i18n = i18n
          )
        },
        indicators = mort_indicators
      )

      tabPanelsServer(
        "panel1",
        serverInput = function(id, current_indicator) {
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = reactive(cache()$mortality_summary),
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) {
              
              # Extract dynamic elements for the title (Country and Year Range)
              country_name <- attr(d, 'country') %||% ""
              min_yr <- min(d$year, na.rm = TRUE)
              max_yr <- max(d$year, na.rm = TRUE)
              
              # Build the translated title and append the dynamic country/years
              base_title <- i18n$t(paste0("title_", current_indicator))
              full_title <- paste0(base_title, ", ", country_name, ", ", min_yr, "-", max_yr, ".")
              
              plot_mortality_plausibility(
                d, 
                indicator = current_indicator,
                title = full_title,
                y_axis = i18n$t(paste0("ylab_", current_indicator)),
                x_axis = i18n$t("title_global_year"),
                note = i18n$t(paste0("note_", current_indicator)),
                legend_labels = list(
                  median = i18n$t("lbl_median"),
                  plausible = i18n$t("lbl_plausible")
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = subnational_mort_indicators
      )

      countdownHeaderServer(
        'mortality',
        cache = cache,
        path = '9-mortality',
        i18n = i18n
      )
    }
  )
}
