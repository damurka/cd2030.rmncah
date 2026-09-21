consistencyCheckUI <- function(id, i18n) {
  ns <- NS(id)

  tabBox(
    title = i18n$t("title_consist_checks"),
    width = 12,
    tabPanel(
      i18n$t("opt_consist_anc1_penta1"),
      plotDownloadsRowUI(ns("anc1_penta1"))
    ),
    tabPanel(
      i18n$t("opt_consist_penta1_penta3"),
      plotDownloadsRowUI(ns("penta1_penta3"))
    ),
    tabPanel(
      i18n$t("opt_consist_custom"),
      fluidRow(
        column(3, indicatorSelect(ns("x_label"), i18n, label = "lbl_axis_x_consist")),
        column(3, offset = 1, indicatorSelect(ns("y_label"), i18n, label = "lbl_axis_y_consist")),
        column(12, plotDownloadsRowUI(ns("custom_graph")))
      )
    )
  )
}

consistencyCheckServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      calculateRatiosServer(id, cache, i18n)

      x_label <- indicatorSelectServer("x_label")
      y_label <- indicatorSelectServer("y_label")
      legend <- reactive({
        c(
          district = i18n$t("lbl_leg_consist_district"),
          linear_fit = i18n$t("lbl_leg_consist_linear_fit"),
          diagonale = i18n$t("lbl_leg_consist_diagonal")
        )
      })

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      plotDownloadsRowServer(
        id = "anc1_penta1",
        i18n = i18n,
        plot_data = data,
        plot_filename = reactive("anc1_penta1_plot"),
        plot_fun = function(d) {
          vacc1 <- i18n$t("opt_anc1")
          vacc2 <- i18n$t("opt_penta1")
          plot_comparison_anc1_penta1(
            d,
            title = str_glue(i18n$t("plt_title_consist_checks")),
            x_label = vacc1,
            y_label = vacc2,
            legend = legend()
          )
        },
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("ratio_plot")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet_name_1, d)
        }
      )

      plotDownloadsRowServer(
        id = "penta1_penta3",
        i18n = i18n,
        plot_data = data,
        plot_filename = reactive("penta1_penta3_plot"),
        plot_fun = function(d) {
          vacc1 <- i18n$t("opt_penta1")
          vacc2 <- i18n$t("opt_penta3")
          plot_comparison_penta1_penta3(
            d,
            title = str_glue(i18n$t("plt_title_consist_checks")),
            x_label = vacc1,
            y_label = vacc2,
            legend = legend()
          )
        },
        excel_write_fun = function(wb, d) {
          sheet_name_1 <- i18n$t("ratio_plot")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet_name_1, d)
        }
      )

      plotDownloadsRowServer(
        id = "custom_graph",
        i18n = i18n,
        plot_data = data,
        plot_filename = reactive(paste0(x_label(), "_", y_label(), "_plot")),
        plot_fun = function(d) {
          req(x_label(), y_label())
          vacc1 <- i18n$t(paste0("opt_", x_label()))
          vacc2 <- i18n$t(paste0("opt_", y_label()))
          plot_comparison(
            d,
            x_label(),
            y_label(),
            title = str_glue(i18n$t("plt_title_consist_checks")),
            x_label = vacc1,
            y_label = vacc2,
            legend = legend()
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
