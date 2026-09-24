mort_indicators <- c('mmr_inst', 'sbr_inst', 'nn_inst')
subnational_mort_indicators <- c("fresh_total_sb", "ratio_md_sb", "ratio_md_nd")

mortality_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_tabbed_charts_ui(ns("panel"), i18n, "title_mortality_institutional", cd_coverage_plot_ui, 
                indicators = mort_indicators, showCustom = FALSE),
    cd_tabbed_charts_ui(ns("panel1"), i18n, "title_subnational_mortality_institutional", cd_coverage_plot_ui, 
                indicators = subnational_mort_indicators, showCustom = FALSE)
  )
}

mortality_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why. cache()$mortality_summary
      # is expensive (~1s, see mortality_mapping_server()'s own active gate) and is read by every one of the 6
      # indicator tabs below, so gating it once here saves the same recomputation 6 times over at startup.
      mortality_summary <- reactive({
        req(cache(), active())
        cache()$mortality_summary
      })

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {

          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = mortality_summary,
            sheet_name = reactive(current_indicator),
            plot_fun = function(d) plot(d, indicator = current_indicator),
            i18n = i18n
          )
        },
        indicators = mort_indicators,
        showCustom = FALSE
      )

      cd_tabbed_charts_server(
        "panel1",
        serverInput = function(id, current_indicator) {

          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = mortality_summary,
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
        indicators = subnational_mort_indicators,
        showCustom = FALSE
      )

    }
  )
}
