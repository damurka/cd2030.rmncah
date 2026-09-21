denominatorInputUI <- function(id, i18n, allow_input = FALSE, is_maternal = FALSE) {
  ns <- NS(id)
  if (allow_input) {
    choices <- c(
      "opt_dhis2" = "dhis2",
      "opt_anc1" = "anc1",
      "opt_penta1" = "penta1",
      "opt_penta1derived" = "penta1derived",
      "opt_anc1derived" = "anc1derived"
    )
    i18nSelectizeInput(
      ns("denominator"),
      label = if (is_maternal) "title_denom_select_best_mat" else "title_denom_select_best_vacc",
      tooltip = if (is_maternal) "tt_denom_select_best_mat" else "tt_denom_select_best_vacc",
      choices = choices
    )
  } else {
    uiOutput(ns('selected_denominator'))
  }
}

denominatorInputServer <- function(id, cache, i18n, label = 'title_denom_select_best', allowInput = FALSE, is_maternal = FALSE, display_mode = 'combined') {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      denominator <- reactive({
        req(cache())
        if (is_maternal) {
          cache()$maternal_denominator
        } else {
          cache()$denominator
        }
      })

      observe({
        req(denominator())

        if (is.null(input$denominator) || input$denominator != denominator()) {
          updateI18nSelectizeInput(session, 'denominator', selected = denominator())
        }

        if (!allowInput) {
          runjs(str_glue("$('#{ns('denominator')}')[0].selectize.lock();"))
        }
      })

      observeEvent(input$denominator, {
        req(cache(), input$denominator, allowInput)
        if (input$denominator == "") return()

        if (is_maternal) {
          isolate(cache()$set_maternal_denominator(input$denominator))
        } else {
          isolate(cache()$set_denominator(input$denominator))
        }
      })

      output$selected_denominator <- renderUI({
        req(cache())

        translate_or <- function(key, fallback) {
          val <- i18n$t(key)
          if (is.null(val) || identical(val, key)) fallback else val
        }

        denom_text <- function(code) {
          key <- paste0('opt_', code)
          val <- i18n$t(key)
          if (is.null(val) || identical(val, key)) toupper(code) else val
        }

        if (identical(display_mode, 'single')) {
          value_code <- if (is_maternal) cache()$maternal_denominator else cache()$denominator
          default_label <- if (is_maternal) 'Maternal denominator' else 'Vaccination denominator'

          return(tags$div(
            class = 'form-group',
            tags$label(class = 'control-label', translate_or(label, default_label)),
            tags$div(class = 'denominator-readonly-control', denom_text(value_code))
          ))
        }

        vax_label <- translate_or('opt_vacc', 'Vaccination')
        mat_label <- translate_or('title_global_maternal', 'Maternal')

        tags$div(
          class = 'form-group denominator-summary-group',
          tags$label(class = 'control-label', translate_or(label, 'Selected denominator')),
          tags$div(
            class = 'denominator-summary-control',
            tags$div(class = 'denominator-row',
              tags$span(class = 'denominator-row-label', paste0(vax_label, ':')),
              tags$span(class = 'denominator-row-value', denom_text(cache()$denominator))
            ),
            tags$div(class = 'denominator-row',
              tags$span(class = 'denominator-row-label', paste0(mat_label, ':')),
              tags$span(class = 'denominator-row-value', denom_text(cache()$maternal_denominator))
            )
          )
        )
      })
    }
  )
}

