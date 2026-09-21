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

#' @title Analysis options box
#' @param optionsTitle Title shown on the box (translated)
#' @param ... UI components (e.g., input UIs)
countdownOptions <- function(title, ...) {
  box(
    title = title,
    status = 'success',
    width = 12,
    solidHeader = TRUE,
    class = 'cd-options-card',
    fluidRow(...)
  )
}
