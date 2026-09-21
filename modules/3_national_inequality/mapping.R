subnationalMappingUI <- function(id, i18n) {
  ns <- NS(id)
  tabPanelsUI(ns("maps"), i18n, "title_mapping_subnational", downloadCoverageUI)
}

subnationalMappingServer <- function(id, cache, i18n, palette, selected_tab) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(palette))
  stopifnot(is.reactive(selected_tab))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      tabPanelsServer(
        "maps",
        serverInput = function(id, current_indicator) {
          denom_rx <- reactive({
            req(cache())
            cache()$get_denominator(current_indicator)
          })
          data_rx <- reactive({
            req(cache())
            cache()$get_filtered_mapping_data(current_indicator, "adminlevel_1", palette())
          })

          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_adminlevel_1_map_", denom_rx())),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              indicator <- i18n$t(paste0("opt_", current_indicator))
              plot(d,
                   title = str_glue(i18n$t("plt_title_map_dist")),
                   caption = i18n$t("plt_caption_map_dhis2"),
                   legend = str_glue(i18n$t("lbl_axis_y_coverage"))
              )
            },
            i18n = i18n
          )
        },
        selected_tab = selected_tab
      )
    }
  )
}
