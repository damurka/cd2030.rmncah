inequalityUI <- function(id, i18n) {
  ns <- NS(id)

  tabPanelsUI(ns("inequality"), i18n, "title_inequ_national", downloadCoverageUI)
}

inequalityServer <- function(id, cache, i18n, admin_level, region = reactive(NULL)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(admin_level))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      indicator <- indicatorSelectServer("indicator")

      selected_tab <- tabPanelsServer(
        "inequality",
        serverInput = function(id, current_indicator) {
          denom_rx <- reactive({
            req(cache())
            cache()$get_denominator(current_indicator)
          })
          data_rx <- reactive({
            req(cache())
            cache()$get_filtered_inequality(indicator = current_indicator, admin_level = admin_level(), region = region())
          })

          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_", admin_level(), "_inequality_", denom_rx())),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              indicator <- i18n$t(paste0("opt_", current_indicator))
              subtitle <- if (admin_level() == 'adminlevel_1' && is.null(region())) {
                'lbl_inequ_subnat_admin1'
              } else if (admin_level() == 'adminlevel_1' && !is.null(region())) {
                'lbl_inequ_subnat_region'
              } else if (admin_level() == 'district' && is.null(region())) {
                'lbl_inequ_subnat_district'
              } 
              labels <- if (is.null(region())) "title_coverage_national" else "lbl_inequ_coverage_region"
              region <- region()
              denominator <- i18n$t(paste0("opt_", denom_rx()))
              plot(d,
                   title = indicator,
                   subtitle = str_glue(i18n$t(subtitle)),
                   x_axis = i18n$t("title_global_year"),
                   y_axis = str_glue(i18n$t("lbl_axis_y_coverage")),
                   caption = i18n$t(paste0("plt_caption_equity_", denom_rx())),
                   legend_labels = list(
                     subnational = i18n$t("lbl_inequ_coverage_subnat"),
                     national = str_glue(i18n$t(labels))
                   )
              )
            },
            i18n = i18n
          )
        }
      )

      return(selected_tab)
    }
  )
}
