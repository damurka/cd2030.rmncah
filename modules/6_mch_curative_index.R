mch_curative_index_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_chart_card(
      title = i18n$t("title_mch_curative"),
      chart_toolbar = cd_plot_toolbar_ui(ns("mch_curative")),
      i18n = i18n,
      status = "success",
      width = 12,
      cd_plot_ui(ns("mch_curative"), toolbar_inline = TRUE)
    )
  )
}

mch_curative_index_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
      plot_data <- reactive({
        req(cache(), active())
        cache()$generate_admin1_mch_curative_index()
      })

      cd_plot_server(
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
          cd_add_sheet(wb, i18n$t("title_mch_curative"), d)
        }
      )

    }
  )
}
