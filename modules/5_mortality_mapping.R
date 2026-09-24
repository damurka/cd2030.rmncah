mort_map_indicators <- c('mmr_inst', 'sbr_inst')

mortality_mapping_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_map_options(
      cd_chip_multi(ns('years'), "title_global_select_years", i18n = i18n),
      cd_palette_chip(ns("palette"), i18n, first = "Reds")
    ),

    cd_tabbed_charts_ui(ns("panel"), i18n, "title_mortality_mapping", cd_coverage_plot_ui, 
                indicators = mort_map_indicators, showCustom = FALSE)
  )
}

mortality_mapping_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          
          data_rx <- reactive({
            req(cache(), cache()$check_mortality_params)
            req(input$palette)
            cache()$filter_mortality_summary(str_remove(current_indicator, '_inst'), palette = input$palette)
          })
          
          cd_coverage_plot_server(
            id = id, # or just ind if inside the same module id
            filename = reactive(current_indicator),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0('opt_', current_indicator))),
            plot_fun = function(d) plot(d),
            i18n = i18n
          )
        },
        indicators = mort_map_indicators,
        showCustom = FALSE
      )

      # `mortality_summary` is computed on demand and takes about a second, and it is invalidated by every data
      # adjustment. Wait until this page is open instead of recomputing it in the background.
      # (A separate req(): req(a, b) evaluates every argument before checking any of them.)
      mortality_years <- reactive({
        req(active())
        req(cache(), cache()$mortality_summary)

        cache()$mortality_summary %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)
      })

      cd_years_sync(input, session, "years",
        years = mortality_years,
        selected = reactive({ req(cache()); cache()$mortality_mapping_years })
      )

      observeEvent(input$years, {
        req(cache())
        cache()$set_mortality_mapping_years(cd_years_input(input$years, cache()$data_years))
      })

    }
  )
}
