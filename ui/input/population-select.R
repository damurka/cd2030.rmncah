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
    choices = choices,
    # starts empty: the value comes from the cache, and an empty value is ignored below, so mounting never
    # overwrites the stored choice with the first option
    selected = ""
  )
}

populationSelectServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(id = id, module = function(input, output, session) {
    mounted <- cdMounted(input, "population")

    # cache -> chip, once the chip exists
    observeEvent(list(cache(), mounted()), {
      req(cache(), mounted())
      updateI18nSelectizeInput(session, "population", selected = cache()$derivation_population)
    })

    # chip -> cache
    observeEvent(input$population, {
      req(cache(), input$population)
      cache()$set_derivation_population(input$population)
    })

    return(reactive(input$population))
  })
}
