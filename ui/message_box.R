messageBoxUI <- function(id, label = NULL, width = NULL) {
  ns <- NS(id)

  dep <- htmlDependency(
    name = "messagebox",
    version = "0.1.0",
    src = c(href = "messagebox"),       # place JS at www/messagebox/messagebox.js
    script = "messagebox.js"
  )

  ui <- div(
    id = ns("container"),
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) css(width = validateCssUnit(width)),

    if (!is.null(label))
      tags$label(class = "control-label", `for` = ns("body"), label),

    div(
      id = ns("body"),
      class = "shiny-input-wrapper",
      `aria-live` = "polite",
      `aria-atomic` = "false",

      tags$div(
        id = ns("messages"),
        class = "messages-stack",
        style = "display:flex; flex-direction:column; gap:8px; max-height:240px; overflow:auto"
      )
    ),

    singleton(tags$style(HTML(sprintf("
      #%s .msg { font-weight:600; border:1px solid; padding:10px; border-radius:4px; white-space:pre-wrap }
      #%s .msg.info { color:#6c757d; border-color:#6c757d }
      #%s .msg.success { color:#155724; border-color:#155724 }
      #%s .msg.error { color:#721c24; border-color:#721c24 }
      #%s .msg.warning { color:#856404; border-color:#856404 }
      #%s .help-block { margin-top:5px }
    ",
      ns("container"),
      ns("container"),
      ns("container"),
      ns("container"),
      ns("container"),
      ns("container")
    ))))
  )

  attachDependencies(ui, dep, append = TRUE)
}

messageBoxServer <- function(id, i18n = NULL, default_message = "msg_upload_awaiting", use_pre = FALSE, help_text = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    stack_sel <- paste0("#", ns("messages"))
    statuses <- c("info", "success", "error", "warning")

    tr <- function(key, parameters = NULL) {
      if (is.null(parameters)) parameters <- list()
      if (!is.null(i18n) && is.function(i18n$t)) {
        str_glue_data(parameters, i18n$t(key))
      } else {
        str_glue_data(parameters, key)
      }
    }

    send_msg <- function(action, text = NULL, status = "info") {
      session$sendCustomMessage(
        "messagebox",
        list(
          rootId = ns("body"),
          action = action,
          text = text,
          status = status,
          usePre = isTRUE(use_pre)
        )
      )
    }

    make_node <- function(text, status, parameters) {
      if (!status %in% statuses) stop(tr("err_global_invalid_status"))
      tr(text, parameters)
    }

    clear_messages <- function() {
      send_msg("clear")
    }

    add_message <- function(message, status = "info", parameters = NULL) {
      txt <- make_node(message, status, parameters)
      send_msg("add", txt, status)
    }

    update_message <- function(message, status = "info", parameters = NULL) {
      clear_messages()
      add_message(message, status, parameters)
    }

    if (!is.null(help_text)) {
      insertUI(
        selector  = paste0("#", ns("container")),
        where     = "beforeEnd",
        immediate = TRUE,
        ui        = tags$span(class = "help-block", tr(help_text))
      )
    }

    add_message(default_message, "info")

    list(
      add_message = add_message,
      update_message = update_message,
      clear_messages = clear_messages
    )
  })
}
