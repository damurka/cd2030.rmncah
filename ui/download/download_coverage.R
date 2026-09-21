downloadCoverageUI <- function(id) {
  ns <- NS(id)
  plotDownloadsRowUI(ns("plot"))
}

downloadCoverageServer <- function(id, filename, data_fn, ..., sheet_name, i18n, plot_fun = NULL) {
  stopifnot(is.reactive(data_fn))
  stopifnot(is.reactive(filename))
  stopifnot(is.reactive(sheet_name))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      plotDownloadsRowServer(
        id = "plot",
        i18n = i18n,
        plot_data = data_fn,
        plot_filename = filename,
        plot_fun = if (is.null(plot_fun)) function(d) plot(d, ...) else plot_fun,
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- sheet_name()
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = d %>% select(-any_of("geometry")), startCol = 1, startRow = 1)
        }
      )
    }
  )
}
