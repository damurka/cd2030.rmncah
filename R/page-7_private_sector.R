private_indicators <- c('national', 'area')

private_sector_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_chart_card(
      title = i18n$t('title_private_sector'),
      chart_toolbar = cd_plot_toolbar_ui(ns("national")),
      i18n = i18n,
      width = 12,
      status = "success",
      collapsible = TRUE,

      cd_plot_ui(ns("national"), toolbar_inline = TRUE)
    ),
    cd_tabbed_charts_ui(ns("panel"), i18n, "title_private_sector", cd_coverage_plot_ui, 
                indicators = private_indicators, showCustom = FALSE)
  )
}

private_sector_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
      # 1. Generate data with translated legend labels
      private_data <- reactive({
        req(cache(), active())
        
        # Pass the translated labels for the stacked bars
        cache()$generate_private_sector_data(
          legend_labels = list(
            Private = i18n$t("lbl_private"),
            NGO     = i18n$t("lbl_ngo"),
            Public  = i18n$t("lbl_public")
          )
        )
      })

      # 2. Render plot with translated title and axes
      cd_plot_server(
        'national',
        i18n,
        plot_data = private_data,
        plot_fun = function(d) {
          plot(
            d,
            title  = i18n$t("title_private_sector_graph"),
            x_axis = i18n$t("xlab_private_sector"),
            y_axis = i18n$t("ylab_private_sector")
          )
        }
      )

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {

          plot_data <- reactive({
            req(cache(), active())
            if (current_indicator == 'national') {
              cache()$national_private_share
            } else {
              cache()$area_private_share
            }
          })
          
          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = plot_data,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) {
              plot(
                d
              )
            },
            i18n = i18n
          )
        },
        indicators = private_indicators,
        showCustom = FALSE
      )

      # 3. Report/Header integration
    }
  )
}
