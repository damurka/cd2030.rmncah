adjustment_indicators <- c('ideliv', "instlivebirths", "penta1", "anc1", 'opd_under5')

adjustmentChangesUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("adjustment"),
    dashboardTitle = i18n$t("title_adjust_changes"),
    i18n = i18n,
    include_report = TRUE,
    tabPanelsUI(ns("panel"), i18n, "title_adjust_visualize", downloadCoverageUI,
      indicators = adjustment_indicators,
      customIndicators = get_all_indicators(),
    )
  )
}

adjustmentChangesServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      indicator <- indicatorSelectServer("indicator")

      data <- reactive({
        req(cache())
        cache()$data_with_excluded_years
      })

      k_factors <- reactive({
        req(cache())

        if (cache()$adjusted_flag) {
          cache()$k_factors
        } else {
          c(anc = 0, ideliv = 0, vacc = 0, opd = 0)
        }
      })

      adjustments <- reactive({
        req(data())
        data() %>%
          generate_adjustment_values(adjustment = "custom", k_factors = k_factors())
      })

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          custom_adjustments <- reactive({
            req(adjustments())
            adjustments() %>%
              filter_adjustment_value(current_indicator)
          })

          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_adjustement_changes")),
            data_fn = custom_adjustments,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              indicator <- i18n$t(paste0("opt_", current_indicator))
              plot(d,
                title = str_glue(i18n$t("plt_title_adjust_comparison")),
                legend_labels = c(
                  raw = str_glue(i18n$t("lbl_adjust_n_before")),
                  adjusted = str_glue(i18n$t("lbl_adjust_n_after"))
                )
              )
            },
            i18n = i18n
          )
        },
        indicators = adjustment_indicators
      )

      countdownHeaderServer(
        "adjustment",
        cache = cache,
        path = "3-data-adjustment",
        # section = "sec-dqa-adjust-outputs",
        i18n = i18n
      )
    }
  )
}
