denominatorInputUI <- function(id, i18n, label = 'title_denominator') {
  ns <- NS(id)
  selectizeInput(
    ns('denominator'),
    label = i18n$t(label),
    choices = c(
      'DHIS2' = 'dhis2',
      'ANC 1' = 'anc1',
      'Penta 1' = 'penta1',
      'Penta 1 Population Growth' = 'penta1derived'
    )
  )
}

denominatorInputServer <- function(id, cache, allowInput = FALSE, maternal = FALSE) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns = session$ns

      denominator <- reactive({
        req(cache())
        if (maternal) cache()$maternal_denominator else cache()$denominator
      })

      observe({
        req(denominator())
        updateSelectizeInput(session, 'denominator', selected = denominator())

        if (!allowInput) {
          runjs(str_glue("$('#{ns('denominator')}')[0].selectize.lock();"))
        }
      })

      observeEvent(input$denominator, {
        req(cache(), input$denominator, allowInput)
        if (maternal) {
          cache()$set_maternal_denominator(input$denominator)
        } else {
          cache()$set_denominator(input$denominator)
        }
      })

      return(denominator)
    }
  )
}
