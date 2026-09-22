downloadButtonUI <- function(id) {
  ns <- NS(id)

  dep <- htmlDependency(
    name = "download-button",
    version = "0.1.0",
    src = c(href = "downloadbtn"), # www/downloadbtn/download-button.js
    script = "download-button.js"
  )

  div(
    class = "cd-download-wrap",
    uiOutput(ns("download_ui")) %>% attachDependencies(dep, append = TRUE)
  )
}

downloadButtonServer <- function(
    id,
    filename,
    extension,
    content,
    data,
    i18n,
    label = "btn_global_download",
    message = "msg_downloading",
    icon = "download",
    icon_only = TRUE,
    tooltip = NULL,
    button_class = "cd-btn",
    size_class = "btn-sm",
    show_when_no_data = FALSE) {
  stopifnot(is.reactive(data), is.reactive(filename), is.reactive(extension))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debounced: during startup the cache's fields can settle over several quick updates, and each one
    # invalidates this. Rendering the button on every tick sends the client "recalculating" faster than it can
    # finish the previous cycle (a Shiny output expects idle -> running -> idle, in order); settling first keeps
    # that in order.
    checked <- shiny::debounce(reactive({
      tryCatch(data(), error = function(e) NULL)
    }), millis = 300)

    icon_tag <- reactive({
      if (inherits(icon, "shiny.tag")) {
        return(icon)
      }
      if (is.character(icon) && length(icon) == 1) {
        return(icon(icon))
      }
      icon("download")
    })

    output$download_ui <- renderUI({
      ok <- !is.null(checked())

      tip <- if (!is.null(tooltip)) tooltip else i18n$t(label)

      downloadButton(
        ns("download_button"),
        label = if (isTRUE(icon_only)) NULL else i18n$t(label),
        icon = icon_tag(),
        class = paste("btn btn-default", size_class, button_class),
        title = tip,
        style = "margin-top:0;",
        disabled = if (!ok) TRUE else NULL
      )
    })

    output$download_button <- downloadHandler(
      filename = function() {
        paste0(filename(), "_", format(Sys.time(), "%Y%m%d%H%M"), ".", extension())
      },
      content = function(file) {
        plot_data <- checked()
        # validate(need(!is.null(plot_data), "No data available to download."))

        session$sendCustomMessage(
          "starting_download",
          list(
            id = ns("download_button"),
            message = i18n$t(message),
            label = i18n$t(label)
          )
        )

        # ensure UI resets even on error
        on.exit(
          {
            session$sendCustomMessage(
              "end_download",
              list(id = ns("download_button"), label = i18n$t(label))
            )
          },
          add = TRUE
        )

        content(file, plot_data)
      }
    )
  })
}
