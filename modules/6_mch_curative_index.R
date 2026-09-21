mchCurativeIndexUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("service_utilization"),
    dashboardTitle = i18n$t("title_mch_curative"),
    i18n = i18n,

    include_report = TRUE,
    
    box(
      title = i18n$t("title_mch_curative"),
      status = "success",
      width = 12,
      withSpinner(plotDownloadsRowUI(ns("mch_curative")))
    )
  )
}

mchCurativeIndexServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      plot_data <- reactive({
        req(cache())
        cache()$generate_admin1_mch_curative_index()
      })

      plotDownloadsRowServer(
        id = "mch_curative",
        i18n = i18n,
        plot_data = plot_data,
        plot_filename = reactive("mch_curative"),
        plot_fun = function(d) {
          translated_labels <- list(
            title          = i18n$t("lbl_mch_curative_title"),
            x_axis         = i18n$t("lbl_mch_prev_index_x"),
            y_axis         = i18n$t("lbl_curative_index_y"),
            q_top_left     = i18n$t("lbl_quad_low_prev_high_cur"),
            q_top_right    = i18n$t("lbl_quad_high_prev_high_cur"),
            q_bottom_left  = i18n$t("lbl_quad_low_prev_low_cur"),
            q_bottom_right = i18n$t("lbl_quad_high_prev_low_cur")
          )
          plot(d, labels = translated_labels)
        },
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("title_mch_curative")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = d, startCol = 1, startRow = 1)
        }
      )

      countdownHeaderServer(
        "service_utilization",
        cache = cache,
        path = "10-service-utilisation",
        # section = "sec-dqa-overall-score", # You might want to update this ID if the section changed
        i18n = i18n
      )
    }
  )
}
