documentationButtonUI <- function(id, i18n) {
  ns <- NS(id)
  actionButton(
    inputId = ns('document_page'),
    label = i18n$t("btn_doc_add_notes"),
    icon = icon('edit'),
    class ='btn bg-purple btn-flat btn-sm',
    style = 'margin-left:4px;'
  )
}

documentationButtonServer <- function(id, cache, document_objects, page_id, page_name, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$document_page, {
      req(cache(), document_objects)

      objects <- names(document_objects)

      showModal(
        modalDialog(
          title = paste(i18n$t("title_doc_page"), page_name),
          fluidPage(
            fluidRow(
              column(
                6,
                selectInput(
                  inputId = ns('object_select'),
                  label = i18n$t("opt_doc_obj"),
                  choices = c(set_names('', i18n$t("title_doc_select_obj")), objects)
                )
              ),
              column(
                6,
                uiOutput(ns('checkbox_container'))
              )
            ),
            tags$div(
              id = ns('pointers_display'),
              class = 'parameters-section',
              tags$h5(i18n$t("title_doc_interp")),
              uiOutput(ns('pointers_text'))
            ),
            textAreaInput(
              inputId = ns('documentation_text'),
              label = i18n$t("title_doc_enter_notes"),
              placeholder = i18n$t("msg_doc_add"),
              width = '100%',
              height = '150px'
            ),
            tags$div(
              id = ns('parameters_display'),
              class = 'parameters-section',
              tags$h5(i18n$t("title_doc_params")),
              uiOutput(ns('parameters_text'))
            )
          ),
          footer = tagList(
            modalButton(i18n$t("btn_global_cancel")),
            actionButton(ns('save_documentation'), i18n$t("btn_doc_save"), class = 'btn-primary')
          ),
          size = 'l'
        )
      )
    })

    observeEvent(input$object_select, {
      req(cache(), input$object_select)

      selected_object <- document_objects[[input$object_select]]

      params <- map(selected_object$parameters, ~ .x())
      # params <- lapply(selected_object$parameters, function(p) p())

      output$parameters_text <- renderUI({
        tags$div(
          style = 'border: 1px solid #ddd; padding: 10px; background: #f9f9f9; border-radius: 5px;',
          tags$ul(
            map(names(params), ~ {
              tags$li(
                tags$b(.x), ': ', tags$span(style = 'color: #007bff;', params[[.x]])
              )
            })
          )
        )
      })

      output$pointers_text <- renderUI({
        prompts <- selected_object$prompts

        if (length(prompts) == 0) {
          tags$em(i18n$t("msg_global_none"))
        } else {
          tags$div(
            style = 'border: 1px solid #ddd; padding: 10px; background: #fefefe; border-radius: 5px;',
            tags$ul(
              map(prompts, ~ tags$li(style = 'margin-bottom: 6px;', i18n$t(.x)))
            )
          )
        }
      })

      output$checkbox_container <- renderUI({
        if (selected_object$always_include) {
          tags$div(
            tags$input(
              type = 'checkbox',
              checked = 'checked',
              disabled = 'disabled',
              style = 'margin-right: 10px;'
            ),
            tags$label(i18n$t("btn_doc_include_page"))
          )
        } else {
          tagList(
            checkboxInput(
              inputId = ns('include_in_report'),
              label = i18n$t("btn_doc_include_final"),
              value = TRUE
            ),
            checkboxInput(
              inputId = ns('include_plot_table'),
              label = i18n$t("btn_doc_include_plot"),
              value = FALSE
            )
          )
        }
      })

      # Check if an existing note matches the parameters and pre-fill fields
      existing_notes <- cache()$get_notes(page_id, input$object_select, params)

      if (nrow(existing_notes) > 0) {
        updateTextAreaInput(session, 'documentation_text', value = existing_notes$note[1])
        updateCheckboxInput(session, 'include_in_report', value = existing_notes$include_in_report[1])
        if (!selected_object$always_include) {
          updateCheckboxInput(session, 'include_plot_table', value = existing_notes$include_plot_table[1])
        }
      } else {
        updateTextAreaInput(session, 'documentation_text', value = '')
        updateCheckboxInput(session, 'include_in_report', value = FALSE)
        if (!selected_object$always_include) {
          updateCheckboxInput(session, 'include_plot_table', value = FALSE)
        }
      }
    })

    observeEvent(input$save_documentation, {
      # Validate required inputs
      req(cache(), input$object_select, input$documentation_text)

      # Retrieve the selected object and its parameters
      selected_object <- document_objects[[input$object_select]]

      # Safely evaluate parameters
      params <- tryCatch(
        map(selected_object$parameters, ~ .x()),
        error = function(e) {
          showNotification(i18n$t("err_doc_eval_params"), type = 'error')
          return(NULL)
        }
      )
      req(params)  # Stop execution if parameter evaluation fails

      # Determine whether the note should be included in the report
      include_in_report <- if (selected_object$always_include) {
        TRUE
      } else {
        input$include_in_report
      }

      include_plot_table <- if (!selected_object$always_include) {
        input$include_plot_table
      } else {
        NULL
      }

      # Save the note to the cache
      tryCatch({
        # Use append_page_note with overwrite parameter
        cache()$append_page_note(
          page_id = page_id,
          object_id = input$object_select,
          note = input$documentation_text,
          parameters = params,
          single_entry = selected_object$single_entry,  # Overwrite for single-entry objects
          include_in_report = include_in_report,
          include_plot_table = include_plot_table
        )

        # Close modal and show success notification
        removeModal()
        showNotification(i18n$t("msg_doc_saved"), type = 'message')
      }, error = function(e) {
        # Handle errors during the save process
        showNotification(i18n$t("err_doc_save_failed"), type = 'error')
        message('Error saving documentation: ', clean_error_message(e))  # Log error details
      })
    })

  })
}
