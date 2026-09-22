plotDownloadsRowUI <- function(id) {
  ns <- NS(id)

  div(
    class = "cd-plot-wrap",
    withSpinner(plotCustomOutput(ns("plot"))),
    div(
      class = "cd-toolbox",
      # tools for this chart only (see ui/react/chart-options.R)
      cdChartLabels(ns("labels")),
      cdChartView(ns("view")),
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
      # The plot as its function draws it. The chart tools then change how it is drawn, never what it shows.
      plot_obj <- reactive({
        req(plot_data())
        plot_fun(plot_data())
      })

      # `input$labels` / `input$view`: what the user changed for this chart (NULL when nothing)
      layout <- reactive(chart_layout(plot_obj(), input$view))
      final_plot <- reactive(apply_chart_options(plot_obj(), input$labels, input$view, layout()))

      output$plot <- renderCustomPlot(
        {
          p <- plot_obj()
          lay <- layout()

          # Tell the tools about this chart: its own text (placeholders in the label editor) and what Auto did.
          # This runs inside the render on purpose: a render only runs for a chart that is on screen, so charts on
          # pages nobody has opened are never built just to fill in a tool. The tools are mounted by then.
          editable <- inherits(p, "ggplot")
          updateCdChip("labels", session, editable = editable,
                       defaults = if (editable) chart_label_defaults(p, flipped = lay$flipped) else list())
          updateCdChip("view", session,
                       autoNote = if (lay$auto && lay$flip) cdText(i18n, "lbl_chart_auto_down") else "")

          final_plot()
        },
        height = function() tryCatch(layout()$height, error = function(e) 400)
      )

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
          # The image carries the same label and view changes as the screen.
          drawn <- plot_fun(d)
          lay <- chart_layout(drawn, isolate(input$view))
          p <- apply_chart_options(drawn, isolate(input$labels), isolate(input$view), lay)
          height_px <- max(2160, round(lay$height / 400 * 2160 * 0.75))
          if (inherits(p, "ggplot")) {
            ggsave(file, plot = p, width = 3840, height = height_px, dpi = 300, units = "px")
          } else {
            # a base-graphics plot: it has just been drawn, so save what is on the device
            ggsave(file, width = 3840, height = 2160, dpi = 300, units = "px")
          }
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
