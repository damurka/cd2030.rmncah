mort_compl_indicators <- c('mmr_ratio', 'sbr_ratio')

mortality_completeness_ui <- function(id, i18n) {
  ns <- NS(id)

  cd_page_ui(id, i18n,
    cd_tabbed_charts_ui(ns("panel"), i18n, "title_mortality_completeness", cd_coverage_plot_ui, 
                indicators = mort_compl_indicators, showCustom = FALSE)
  )
}

mortality_completeness_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(active))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      cd_tabbed_charts_server(
        "panel",
        serverInput = function(id, current_indicator) {
          # active(): see coverage_server() in modules/3_national_coverage/coverage.R for why.
          data_rx <- reactive({
            req(cache(), active(), cache()$check_inequality_params)
            cache()$summarise_completeness_ratio(str_remove(current_indicator, '_ratio'))
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
        indicators = mort_compl_indicators,
        showCustom = FALSE
      )

    }
  )
}
