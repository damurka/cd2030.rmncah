#' @title Content Dashboard UI wrapper
#' @description Combines a page header, optional input box, and tab panels
#' @param dashboardId Unique module ID
#' @param dashboardTitle Localized dashboard title
#' @param i18n Translator object
#' @param countdownOptions Optional box() with UI inputs
#' @param ... Tab panels passed to tabBox()
countdownDashboard <- function(dashboardId,
                             dashboardTitle,
                             i18n,
                             ...,
                             countdownOptions = NULL,
                             include_report = FALSE,
                             include_notes = FALSE,
                             include_help = TRUE) {

  tagList(
    # Header section with title and standard buttons
    countdownHeader(
      id = dashboardId,
      title = dashboardTitle,
      i18n = i18n,
      include_report = include_report,
      include_notes = include_notes,
      include_help = include_help
    ),

    # Main dashboard content with optional options box and tab panels
    countdownBody(
      countdownOptions,
      ...
    )
  )

}

#' @title Analysis options
#' @description A page's options as one line of filter chips (see cdFilterBar()). Pages still pass their inputs
#'   wrapped in column()/fluidRow() as they always did; the wrappers are dropped, because the chips lay
#'   themselves out.
#' @param title Kept for compatibility; the bar labels its chips instead of carrying a title.
#' @param ... UI components (e.g., input UIs)
countdownOptions <- function(title, ...) {
  # list2() rather than list(): pages leave a trailing comma in their argument lists, which fluidRow() tolerated
  do.call(cdFilterBar, c(cd_unwrap_layout(rlang::list2(...)), list(i18n = cd_i18n())))
}

# The inputs inside column() / fluidRow() wrappers and tag lists, flattened
cd_unwrap_layout <- function(items) {
  out <- list()
  for (item in items) {
    if (is.null(item)) next
    is_tag <- inherits(item, "shiny.tag")
    cls <- if (is_tag) paste(item$attribs$class, collapse = " ") else ""
    if (is_tag && (grepl("(^| )col-sm-[0-9]+", cls) || grepl("(^| )row( |$)", cls))) {
      out <- c(out, cd_unwrap_layout(item$children))
    } else if (!is_tag && is.list(item) && !inherits(item, "shiny.tag.list")) {
      out <- c(out, cd_unwrap_layout(item))
    } else if (inherits(item, "shiny.tag.list")) {
      out <- c(out, cd_unwrap_layout(unclass(item)))
    } else {
      out <- c(out, list(item))
    }
  }
  out
}
