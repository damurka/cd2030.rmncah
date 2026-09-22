# A filter chip with the arguments the app has always used for translated dropdowns, so call sites do not change.
# It used to build a <select>; it now builds a React chip (js/src/components, ui/react/cd-react.R). The chip's
# text is translated by the component itself, so nothing here waits for the browser.
#
#   label, tooltip  translation keys (or the markup i18n$t() returns for one)
#   choices         a named vector: names are translation keys, values are option values. NULL when the options
#                   are data and arrive later through updateI18nSelectizeInput().
#   multiple        a multi-choice chip; choosing nothing means "all" and reaches the server as ""
#   width, nonempty accepted for compatibility; a chip sizes itself
i18nSelectizeInput <- function(inputId, label,
                               choices = NULL,
                               selected = NULL,
                               tooltip = NULL,
                               multiple = FALSE,
                               width = NULL,
                               nonempty = FALSE) {
  if (!is.null(choices) && length(choices) > 0 &&
    (is.null(names(choices)) || anyNA(names(choices)))) {
    stop("choices must be a named vector/list: names = translation keys, values = option values")
  }

  i18n <- cd_i18n()
  options <- if (is.null(choices)) list() else cdOptions(choices, i18n)

  if (multiple) {
    cdChipMulti(inputId, label, i18n = i18n, selected = selected, hint = tooltip, options = options)
  } else {
    cdChipSelect(inputId, label, i18n = i18n, selected = selected, hint = tooltip, options = options)
  }
}

# Change a chip's options and/or its value. The chip must have mounted (see cdMounted()).
updateI18nSelectizeInput <- function(session, inputId,
                                     choices = NULL,
                                     selected = NULL) {
  props <- list()

  if (!is.null(choices)) {
    if (length(choices) > 0 &&
      (is.null(names(choices)) || anyNA(names(choices)) || any(names(choices) == ""))) {
      stop("choices must be a named vector: names = translation keys")
    }
    props$options <- cdOptions(choices, cd_i18n())
  }

  if (!is.null(selected)) {
    props$value <- as.character(selected)
  }

  if (length(props) == 0) {
    return(invisible(NULL))
  }

  updateCdChip(inputId, session, !!!props)
}
