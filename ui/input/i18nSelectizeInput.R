i18nSelectizeInput <- function(inputId, label,
                               choices = NULL,
                               selected = NULL,
                               tooltip = NULL,
                               multiple = FALSE,
                               width = NULL,
                               nonempty = FALSE) {
  # Restore previous value if it exists
  selected <- restoreInput(id = inputId, default = selected)

  sel <- if (is.null(selected)) character(0) else as.character(selected)

  # ---- Build options (only if choices provided) ----
  option_tags <- NULL

  if (!is.null(choices)) {
    if (length(choices) > 0 &&
      (is.null(names(choices)) || anyNA(names(choices)))) {
      stop("choices must be a named vector/list: names = translation keys, values = option values")
    }

    values <- unname(choices)
    keys <- names(choices)

    option_tags <- pmap(
      list(v = values, k = keys),
      function(v, k) {
        tags$option(
          value = v,
          class = "i18n",
          `data-key` = k,
          if (v %in% sel) `selected` <- "selected" else NULL,
          k
        )
      }
    )
  }

  # ---- Build <select> ----
  select_tag <- tags$select(
    id = inputId,
    class = "shiny-input-select form-control",
    option_tags
  )

  if (multiple) select_tag$attribs$multiple <- "multiple"

  container <- div(
    class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    if (!is.null(tooltip)) {
      tooltip_label(label, tooltip)
    } else {
      tags$label(`for` = inputId, class = "control-label i18n", `data-key` = label, label)
    },
    div(select_tag)
  )
}

updateI18nSelectizeInput <- function(session, inputId,
                                     choices = NULL,
                                     selected = NULL) {
  msg <- list()

  if (!is.null(choices)) {
    if (length(choices) > 0 &&
      (is.null(names(choices)) || anyNA(names(choices)) || any(names(choices) == ""))) {
      stop("choices must be a named vector: names = translation keys")
    }

    msg$options <- imap(
      choices,
      function(v, k) {
        list(
          value = v,
          text = k,
          class = "i18n",
          `data-key` = k
        )
      }
    )
  }

  if (!is.null(selected)) {
    msg$value <- as.character(selected)
  }

  if (length(msg) == 0) {
    return(invisible(NULL))
  }

  session$sendInputMessage(inputId, msg)
}
