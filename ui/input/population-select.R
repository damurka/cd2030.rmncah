populationSelect <- function(id) {
  ns <- NS(id)

  choices <- c(
    "opt_denom_live_births_dhis2" = "totlivebirths_dhis2",
    "opt_denom_total_births_dhis2" = "totbirths_dhis2",
    # "opt_denom_total_births_un" = "un_births",
    "opt_denom_total_pop_dhis2" = "totpop_dhis2",
    # "opt_denom_total_pop_un" = "un_population",
    "opt_denom_under1_dhis2" = "totunder1_dhis2"#,
    # "opt_denom_under1_un" = "un_under1"
  )
  i18nSelectizeInput(
    ns("population"),
    label = "title_denom_pop_select",
    tooltip = "tt_denom_pop_select",
    choices = choices
  )
}

populationSelectServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(id = id, module = function(input, output, session) {
    observeEvent(cache(), {
      req(cache())
      updateI18nSelectizeInput(session, "population", selected = cache()$derivation_population)
    })

    observe({
      req(cache(), input$population)
      cache()$set_derivation_population(input$population)
    })

    return(reactive(input$population))
  })
}
