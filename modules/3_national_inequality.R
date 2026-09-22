source("modules/3_national_inequality/inequality.R")
source("modules/3_national_inequality/mapping.R")

nationalInequalityUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("national_inequality"),
    dashboardTitle = i18n$t("title_inequ_national"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(6, denominatorInputUI(ns("denominator"), i18n)),
      column(3, cdChipMulti(ns("years"), "title_global_select_years", i18n = i18n)),
      column(3, i18nSelectizeInput(ns("palette"), 
                                   label = i18n$t("title_global_palette"), 
                                   choices = c("opt_palette_greens" = "Greens", "opt_palette_blues" = "Blues", "opt_palette_reds" = "Reds")))
    ),
    inequalityUI(ns("inequality"), i18n),
    subnationalMappingUI(ns("map"), i18n)
  )
}

nationalInequalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      denominatorInputServer("denominator", cache, i18n)

      selected_tab <- inequalityServer("inequality", cache, i18n, reactive('adminlevel_1'))
      subnationalMappingServer("map", cache, i18n, reactive(input$palette), selected_tab)

      yearsSelectSync(input, session, "years",
        years = reactive({ req(cache()); cache()$data_years }),
        selected = reactive({ req(cache()); cache()$mapping_years })
      )

      observeEvent(input$years, {
        req(cache())
        cache()$set_mapping_years(as.integer(input$years))
      })

      countdownHeaderServer(
        "national_inequality",
        cache = cache,
        path = "6-equity-analysis",
        i18n = i18n
      )
    }
  )
}
