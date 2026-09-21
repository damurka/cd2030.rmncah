directoryInput <- function(inputId, label, width = NULL, accept = NULL, buttonLabel = "Browse...", placeholder = "No directory selected") {

  restoredValue <- restoreInput(id = inputId, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }

  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }

  inputTag <- tags$input(
    id = inputId,
    class = "shiny-input-file",
    name = "fileList",
    type = "file",
    webkitdirectory = "true",
    multiple = "true",
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )

  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')

  div(class = "form-group shiny-input-container",
      style = css(width = validateCssUnit(width)),
      tags$label(
        label,
        class = "control-label",
        class = if (is.null(label)) "shiny-label-null",
        style = ' margin-top: 10px;',
        # `id` attribute is required for `aria-labelledby` used by screen readers:
        id = paste0(inputId, "-label"),
        `for` = inputId
      ),

      div(class = "input-group",
          tags$label(class = "input-group-btn input-group-prepend",
                     span(class = "btn btn-default btn-file",
                          buttonLabel,
                          inputTag
                     )
          ),
          tags$input(type = "text", class = "form-control",
                     placeholder = placeholder, readonly = "readonly"
          )
      ),

      tags$div(
        id = paste(inputId, "_progress", sep = ""),
        class = "progress active shiny-file-input-progress",
        tags$div(class = "progress-bar")
      )
  )
}
