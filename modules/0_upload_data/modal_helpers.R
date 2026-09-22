mappingModalUI <- function(id, name) {
  ns <- NS(id)
  actionButton(ns('map'), name)
}

mappingModalServer <- function(id, cache, survey_data, survey_map, title, show_col, type, i18n) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(survey_data))
  stopifnot(is.reactive(survey_map))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      # Reactive for geographic region levels
      gregion_levels <- reactive({
        req(cache())

        cache()$subnational_regions %>%
          distinct(adminlevel_1) %>%
          arrange(adminlevel_1) %>%
          pull(adminlevel_1)
      })

      # Modal trigger on button click
      observeEvent(input$map, {
        req(gregion_levels(), survey_data())

        gregion <- gregion_levels()
        dt <- survey_data() %>%
          distinct(!!rlang::sym(show_col)) %>%
          arrange(!!rlang::sym(show_col)) %>%
          pull(!!rlang::sym(show_col))

        # Show the modal with dynamic UI
        createMappingModal(
          ns = session$ns,
          title = title,
          gregion = gregion,
          choices = dt,
          survey_map = survey_map(),
          show_col = show_col,
          type = type,
          i18n = i18n
        )
      })

      observeEvent(input$save_map, {
        req(cache())
        mapped_to <- map(gregion_levels(), ~ input[[paste0(type, '_', .x)]])
        mapping_result <- tibble(
          admin_level_1 = gregion_levels(),
          !!sym(show_col) := unlist(mapped_to, use.names = FALSE)
        )
        if (type == 'map_mapping') {
          cache()$set_map_mapping(mapping_result)
        } else if (type == 'survey_mapping') {
          cache()$set_survey_mapping(mapping_result)
        }
        removeModal()  # Close the modal dialog
      })
    }
  )
}

createMappingModal <- function(ns, title, gregion, choices, survey_map, show_col, type, i18n) {
  # Helper to get selected choice
  get_selected_choice <- function(region, survey_map, show_col) {
    if (!is.null(survey_map)) {
      survey_map %>%
        filter(admin_level_1 == region) %>%
        pull(!!rlang::sym(show_col)) %>%
        first()
    } else {
      NULL
    }
  }

  # Generate dynamic UI elements for each region
  modal_content <- map(gregion, ~ {
    selected_choice <- get_selected_choice(.x, survey_map, show_col)
    tagList(
      fluidRow(
        column(6, strong(.x)),
        column(6, selectizeInput(
          inputId = ns(paste0(type, '_', .x)),
          label = NULL,
          selected = selected_choice,
          choices = c('Select' = '', choices)
        ))
      ),
      tags$hr(style = 'margin: 0 0 10px;')
    )
  })

  # Show the modal dialog
  showModal(
    modalDialog(
      title = i18n$t(title),
      size = 'l',
      fluidPage(do.call(tagList, modal_content)),
      footer = tagList(
        actionButton(ns('save_map'), i18n$t("btn_upload_save_mapping")),
        modalButton(i18n$t("btn_global_cancel"))
      )
    )
  )
}

