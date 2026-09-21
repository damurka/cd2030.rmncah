indicatorSelect <- function(id, i18n, label = NULL, tooltip = NULL, indicators = NULL, select_all = FALSE) {
  ns <- NS(id)
  label <- if (is.null(label)) "title_global_indicator" else label
  choices <- indicators %||% get_all_indicators()
  if (is.null(choices) || is.null(names(choices))) {
    names(choices) <- paste0("opt_", choices)
  }
  if (select_all) {
    choices <- c("opt_global_select_all" = "", choices)
  }

  i18nSelectizeInput(ns("indicator"),
    label = label,
    choices = choices,
    tooltip = tooltip
  )
}

indicatorSelectServer <- function(id) {
  moduleServer(id = id, module = function(input, output, session) {
    return(reactive(input$indicator))
  })
}
