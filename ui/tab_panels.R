default_indicators <- c('anc4', "instlivebirths", "low_bweight", 'penta3', "measles1")

tabPanelsUI <- function(id, i18n, title_key, uiInput,
                        indicators = NULL,
                        customIndicators = get_analysis_indicators(),
                        showCustom = TRUE) {
  ns <- NS(id)

  indicators <- indicators %||% default_indicators

  tabs <- map(indicators, ~ {
    tabPanel(
      title = i18n$t(paste0("opt_", .x)),
      uiInput(ns(.x))
    )
  })

  custom_tab <- if (isTRUE(showCustom)) {
    tabPanel(
      title = i18n$t("opt_consist_custom"),
      fluidRow(
        column(3, indicatorSelect(ns("indicator"), i18n, indicators = customIndicators)),
        column(12, uiInput(ns("custom")))
      )
    )
  } else {
    NULL
  }

  tabBox(
    id = ns("indicator_tabs"),
    title = i18n$t(title_key),
    width = 12,
    !!!tabs,
    custom_tab
  )
}

tabPanelsServer <- function(id,
                            serverInput,
                            indicators = NULL,
                            selected_tab = reactive(NULL)) {
  stopifnot(is.function(serverInput))
  stopifnot(is.reactive(selected_tab))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      indicators <- indicators %||% default_indicators

      walk(indicators, ~ {
        local({
          id <- .x
          serverInput(id, .x)
        })
      })

      selected <- indicatorSelectServer("indicator")

      observeEvent(selected(),
        {
          req(selected())
          serverInput("custom", selected())
        })

      observeEvent(selected_tab(),
        {
          req(selected_tab())

          updateTabsetPanel(
            session,
            inputId = "indicator_tabs",
            selected = selected_tab() # value we set above
          )
        },
        ignoreInit = TRUE
      )

      return(reactive(input$indicator_tabs))
    }
  )
}
