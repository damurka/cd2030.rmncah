calculateRatiosUI <- function(id, i18n) {
  ns <- NS(id)

  box(
    title = i18n$t("title_consist_ratio_plots"),
    status = "success",
    width = 12,
    plotDownloadsRowUI(ns("ratios"))
  )
}

calculateRatiosServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ratio_summary <- reactive({
        req(cache())
        cache()$ratios_summary
      })

      plotDownloadsRowServer(
        id = "ratios",
        i18n = i18n,
        plot_data = ratio_summary,
        plot_filename = reactive("ratio_plot"),
        plot_fun = function(d) {
          plot(
            d,
            title = i18n$t("plt_title_consist_ratios"),
            # x_axis = NULL,
            # y_axis = NULL,
            x_labels = c(
              "anc1_penta1"   = i18n$t("lbl_consist_ratio_anc1_penta1"),
              "penta1_penta3" = i18n$t("lbl_consist_ratio_penta1_penta3")
            )
          )
        },
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("ratio_plot")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet_name_1, d)
        }
      )
    }
  )
}
