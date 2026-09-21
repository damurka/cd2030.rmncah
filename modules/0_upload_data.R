source("modules/0_upload_data/upload_box.R")
source("modules/0_upload_data/national_rates.R")
source("modules/0_upload_data/file_upload.R")

uploadDataUI <- function(id, i18n, is_electron = FALSE) {
  ns <- NS(id)

  fluidRow(
    uploadBoxUI(ns("upload_box"), i18n, is_electron),
    nationalRatesUI(ns("national_rates"), i18n),
    fileUploadUI(ns("file_uploads"), i18n)
  )
}

uploadDataServer <- function(id, i18n, cdsuite_file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      cache <- reactiveVal()

      nationalRatesServer("national_rates", cache, i18n)
      fileUploadServer("file_uploads", cache, i18n)

      upload_dt <- uploadBoxServer("upload_box", i18n, cdsuite_file)

      observeEvent(upload_dt(), {
        req(upload_dt())
        cache(upload_dt())
      })

      return(cache)
    }
  )
}
