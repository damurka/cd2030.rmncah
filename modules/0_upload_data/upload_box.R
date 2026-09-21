uploadBoxUI <- function(id, i18n, is_electron = FALSE) {
  ns <- NS(id)

  box(
    title = i18n$t("title_upload_main"),
    status = "success",
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      if (!is_electron) {
        column(3, h4(icon("upload"), i18n$t("btn_upload_standard")))
      },
      column(2, offset = if (is_electron) 10 else 7, helpButtonUI(ns("upload_data"), name = i18n$t("btn_global_help")), align = "right")
    ),
    fluidRow(
      column(
        12,
        if (!is_electron) {
          fileInput(
            ns("hfd_file"),
            label = i18n$t("btn_upload_hfd"),
            buttonLabel = i18n$t("lbl_upload_browse"),
            placeholder = "Supported formats: .xls, .xlsx, .dta, .rds",
            accept = c(".xls", ".xlsx", ".dta", ".rds")
          )
        },
        messageBoxUI(ns("feedback"))
      )
    ),
    fluidRow(
      column(4, downloadButtonUI(ns("download_data")))
    )
  )
}

uploadBoxServer <- function(id, i18n, cdsuite_file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      messageBox <- messageBoxServer("feedback", i18n = i18n)

      base_path <- normalizePath(cdsuite_file, winslash = "/", mustWork = FALSE)
      base_dir <- dirname(base_path)

      initial_cache <- reactiveVal(NULL)

      observeEvent(TRUE,
        {
          req(nzchar(base_path))

          if (!file.exists(base_path)) {
            messageBox$update_message("err_upload_failed_general", "error", list(clean_message = "The file provided does not exist"))
            return(NULL)
          }

          tryCatch(
            {
              cache_instance <- load_cache_data(base_path, indicator_group = "rmncah", create_cache = TRUE)$reactive()
              initial_cache(cache_instance())

              messageBox$update_message(
                "msg_upload_success_file", "success",
                list(file_name = base_path)
              )
            },
            error = function(e) {
              initial_cache(NULL)
              clean_message <- clean_error_message(e)
              messageBox$update_message("err_upload_failed_general", "error", list(clean_message = clean_message))
            }
          )
        },
        once = TRUE
      )

      observeEvent(input$hfd_file, {
        req(input$hfd_file)


        file_path <- input$hfd_file$datapath
        file_name <- input$hfd_file$name
        file_type <- input$hfd_file$ext

        valid_types <- c("xls", "xlsx", "dta", "rds")
        if (!file_type %in% valid_types) {
          messageBox$update_message("err_upload_unsupported", "error")
          initial_cache(NULL)
          return()
        }

        tryCatch(
          {
            cache_instance <- load_cache_data(file_path, indicator_group = "rmncah", create_cache = TRUE)$reactive()

            messageBox$update_message("msg_upload_success_file", "success", list(file_name = file_name))

            initial_cache(cache_instance())
          },
          error = function(e) {
            clean_message <- clean_error_message(e)
            messageBox$update_message("err_upload_failed_general", "error", list(clean_message = clean_message))
            initial_cache(NULL)
          }
        )
      })

      downloadButtonServer(
        id = "download_data",
        filename = reactive("master_dataset"),
        extension = reactive("dta"),
        i18n = i18n,
        content = function(file, data) {
          haven::write_dta(initial_cache()$countdown_data, file)
        },
        data = initial_cache,
        label = "btn_upload_download_master"
      )

      helpButtonServer(
        id = "upload_data",
        path = "loading-data",
        cache = initial_cache
      )

      return(reactive(initial_cache()))
    }
  )
}
