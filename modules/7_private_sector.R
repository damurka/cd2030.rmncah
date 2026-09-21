private_indicators <- c('national', 'area')

privateSectorUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('private_sector'),
    dashboardTitle = i18n$t('title_private_sector'),
    i18n = i18n,

    include_report = TRUE,

    box(
      title = i18n$t('title_private_sector'),
      width = 12,
      status = "success",
      collapsible = TRUE,

      plotDownloadsRowUI(ns("national"))
    ),
    tabPanelsUI(ns("panel"), i18n, "title_private_sector", downloadCoverageUI, 
                indicators = private_indicators, showCustom = FALSE)
  )
}

privateSectorServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # 1. Generate data with translated legend labels
      private_data <- reactive({
        req(cache())
        
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
      plotDownloadsRowServer(
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


      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {

          plot_data <- reactive({
            req(cache())
            if (current_indicator == 'national') {
              cache()$national_private_share
            } else {
              cache()$area_private_share
            }
          })
          
          downloadCoverageServer(
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
        indicators = private_indicators
      )

      # 3. Report/Header integration
      countdownHeaderServer(
        'private_sector',
        cache = cache,
        path  = '11-health-system-performance',
        i18n  = i18n
      )
    }
  )
}
