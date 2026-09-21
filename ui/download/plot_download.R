plotDownloadsRowUI <- function(id) {
  ns <- NS(id)

  div(
    class = "cd-plot-wrap",
    withSpinner(plotCustomOutput(ns("plot"))),
    div(
      class = "cd-toolbox",
      downloadButtonUI(ns("download_plot")),
      downloadButtonUI(ns("download_data"))
    )
  )
}

plotDownloadsRowServer <- function(
  id,
  i18n,
  # plot
  plot_data, # reactive
  plot_fun, # function(data) -> draws plot (or returns ggplot object)
  plot_filename = reactive("plot"),
  # plot download
  plot_label_key = "btn_global_download_plot",
  plot_extension = reactive("png"),
  # data download (excel)
  data_label_key = "btn_global_download_data",
  data_extension = reactive("xlsx"),
  excel_write_fun = NULL # function(wb, data) writes workbook
) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      output$plot <- renderCustomPlot({
        req(plot_data())
        plot_fun(plot_data())
      })

      downloadButtonServer(
        id = "download_plot",
        filename = plot_filename,
        extension = plot_extension,
        data = plot_data,
        i18n = i18n,
        label = plot_label_key,
        icon = "camera",
        button_class = "btn-plot",
        content = function(file, d) {
          # If plot_fun returns a ggplot, ggsave will use last_plot() anyway,
          # but safest is to call plot_fun(d) here.
          plot_fun(d)
          ggsave(file, width = 3840, height = 2160, dpi = 300, units = "px")
        }
      )

      if (!is.null(excel_write_fun) && is.function(excel_write_fun)) {
        downloadButtonServer(
          id = "download_data",
          filename = plot_filename,
          extension = data_extension,
          data = plot_data,
          i18n = i18n,
          label = data_label_key,
          icon = "table",
          button_class = "btn-plot",
          content = function(file, d) {
            wb <- createWorkbook()
            excel_write_fun(wb, d) # IMPORTANT: d is already evaluated data (not reactive)
            saveWorkbook(wb, file, overwrite = TRUE)
          }
        )
      }
    }
  )
}
