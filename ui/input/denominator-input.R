denominator_choices <- c(
  "opt_dhis2" = "dhis2",
  "opt_anc1" = "anc1",
  "opt_penta1" = "penta1",
  "opt_penta1derived" = "penta1derived",
  "opt_anc1derived" = "anc1derived"
)

# The chip's label and hint are translation keys
denominator_chip_keys <- function(is_maternal) {
  list(
    label = if (is_maternal) "lbl_chip_denom_mat" else "lbl_chip_denom_vacc",
    hint = if (is_maternal) "tt_denom_select_best_mat" else "tt_denom_select_best_vacc"
  )
}

denominatorInputUI <- function(id, i18n, allow_input = FALSE, is_maternal = FALSE) {
  ns <- NS(id)
  if (allow_input) {
    keys <- denominator_chip_keys(is_maternal)
    # React filter chip (js/src/components/ChipSelect.tsx). It starts empty: the value comes from the
    # cache, and an empty value is ignored by the server below, so mounting never overwrites the cache.
    cdChipSelect(ns("denominator"), keys$label, denominator_choices, i18n, selected = "", hint = keys$hint)
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

      # Only an instance that renders a dropdown has anything to keep in sync with the cache.
      # Read-only instances are just the renderUI below: no observers, and no messages sent to
      # an element that does not exist.
      if (allowInput) {
        denominator <- reactive({
          req(cache())
          if (is_maternal) {
            cache()$maternal_denominator
          } else {
            cache()$denominator
          }
        })

        # A message sent to a chip that has not mounted yet is lost, so wait for it to say it has.
        chip_mounted <- cdMounted(input, "denominator")

        # cache -> chip. Runs when the CACHE value changes, or when the chip mounts. It must not depend on
        # input$denominator: when the user picks something, that change reaches the cache through the writer
        # below, and an observer that also reacted to the input could run first, see the old cache value, and
        # push it back over the user's choice. (Even reading the input in the event expression is enough:
        # observeEvent re-runs whenever anything it reads is invalidated, whether or not the value changed.)
        observeEvent(list(denominator(), chip_mounted()), {
          req(denominator(), chip_mounted())

          if (!identical(isolate(input$denominator), denominator())) {
            updateCdChip("denominator", session, value = denominator())
          }
        })

        # dropdown -> cache: the single writer for this value
        observeEvent(input$denominator, {
          req(cache(), input$denominator)
          if (input$denominator == "") return()

          if (is_maternal) {
            isolate(cache()$set_maternal_denominator(input$denominator))
          } else {
            isolate(cache()$set_denominator(input$denominator))
          }
        })
      }

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

