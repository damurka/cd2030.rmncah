dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}


nationalRatesUI <- function(id, i18n) {
  ns <- NS(id)

  box(
    title = i18n$t("title_upload_national_rates"),
    status = "success",
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, numericInput(ns("anc1_prop"), i18n$t("title_upload_anc1_survey"),
        min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("anc4_prop"), i18n$t("title_upload_anc4_survey"),
                             min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("pregnancy_loss"), i18n$t("title_upload_preg_loss"),
        min = 0, max = 0.05, value = 0, step = 0.001
      )),
      column(3, numericInput(ns("twin_rate"), i18n$t("title_upload_twin_rate"),
        min = 0, max = 0.05, value = 0, step = 0.001
      ))
    ),
    fluidRow(
      column(3, numericInput(ns("neonatal_mortality_rate"), i18n$t("title_upload_nmr"),
                             min = 0, max = 0.05, value = 0, step = 0.001
      )),
      column(3, numericInput(ns("post_neonatal_mortality_rate"), i18n$t("title_upload_pnmr"),
        min = 0, max = 0.05, value = 0, step = 0.001
      )),
      column(3, numericInput(ns("stillbirth_rate"), i18n$t("title_upload_stillbirth"),
        min = 0, max = 0.05, value = 0, step = 0.001
      )),
      column(3, numericInput(ns("ideliv_prop"), i18n$t("title_upload_ideliv_survey"),
        min = 0, max = 100, value = NA, step = 1
      ))
    ),
    fluidRow(
      column(3, numericInput(ns("bcg_prop"), i18n$t("title_upload_bcg_survey"),
                             min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("penta1_prop"), i18n$t("title_upload_penta1_survey"),
        min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("penta3_prop"), i18n$t("title_upload_penta3_survey"),
        min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("measles1_prop"), i18n$t("title_upload_measles1_survey"),
                             min = 0, max = 100, value = NA, step = 1
      ))
    ),
    fluidRow(
      column(3, numericInput(ns("csection_prop"), i18n$t("title_upload_csection_survey"),
        min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("lbw_prop"), i18n$t("title_upload_lbw_survey"),
                             min = 0, max = 100, value = NA, step = 1
      )),
      column(3, numericInput(ns("survey_year"), i18n$t("title_upload_recent_survey_year"),
        min = 2015, max = 2030, value = NA, step = 1
      )),
      column(3, uiOutput(ns("survey_start_ui")))
    )
  )
}

nationalRatesServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      observe({
          req(cache())

          national_estimates <- cache()$national_estimates
          estimates <- cache()$survey_estimates
          survey_year <- cache()$survey_year

          updateNumericInput(session, "neonatal_mortality_rate", value = national_estimates$nmr)
          updateNumericInput(session, "post_neonatal_mortality_rate", value = national_estimates$pnmr)
          updateNumericInput(session, "twin_rate", value = national_estimates$twin_rate)
          updateNumericInput(session, "pregnancy_loss", value = national_estimates$preg_loss)
          updateNumericInput(session, "stillbirth_rate", value = national_estimates$sbr)

          updateNumericInput(session, "anc1_prop", value = unname(estimates["anc1"]))
          updateNumericInput(session, "penta1_prop", value = unname(estimates["penta1"]))
          updateNumericInput(session, "anc4_prop", value = unname(estimates["anc4"]))
          updateNumericInput(session, "ideliv_prop", value = unname(estimates["instlivebirths"]))
          updateNumericInput(session, "bcg_prop", value = unname(estimates["bcg"]))
          updateNumericInput(session, "csection_prop", value = unname(estimates["csection"]))
          updateNumericInput(session, "lbw_prop", value = unname(estimates["low_bweight"]))
          updateNumericInput(session, "penta3_prop", value = unname(estimates["penta3"]))
          updateNumericInput(session, "measles1_prop", value = unname(estimates["measles1"]))

          updateNumericInput(session, "survey_year", value = survey_year)
        }
      )

      observeEvent(c(input$anc1_prop, input$anc4_prop, input$ideliv_prop, input$bcg_prop, input$penta1_prop, input$penta3_prop, input$measles1_prop, input$lbw_prop, input$csection_prop), {
        req(cache())

        estimates <- c(
          anc1 = as.numeric(input$anc1_prop),
          anc4 = as.numeric(input$anc4_prop),
          instlivebirths = as.numeric(input$ideliv_prop),
          bcg = as.numeric(input$bcg_prop),
          penta1 = as.numeric(input$penta1_prop),
          penta3 = as.numeric(input$penta3_prop),
          measles1 = as.numeric(input$measles1_prop),
          low_bweight = as.numeric(input$lbw_prop),
          csection = as.numeric(input$csection_prop)
        )
        
        isolate(cache()$set_survey_estimates(estimates))
      })

      observeEvent(c(input$neonatal_mortality_rate, input$post_neonatal_mortality_rate, input$stillbirth_rate, input$pregnancy_loss, input$twin_rate), {
        req(cache())

        estimates <- list(
          nmr = input$neonatal_mortality_rate,
          pnmr = input$post_neonatal_mortality_rate,
          sbr = input$stillbirth_rate,
          twin_rate = input$twin_rate,
          preg_loss = input$pregnancy_loss
        )
        isolate(cache()$set_national_estimates(estimates))
      })

      observeEvent(input$survey_start_year, {
        req(cache(), input$survey_start_year)
        isolate(cache()$set_start_survey_year(as.numeric(input$survey_start_year)))
      })

      observeEvent(input$survey_year, {
        req(cache(), input$survey_year)
        isolate(cache()$set_survey_year(as.numeric(input$survey_year)))
      })


      observe({
        req(cache())
        survey_year <- cache()$survey_year
        updateNumericInput(session, "survey_year", value = survey_year)
      })

      output$survey_start_ui <- renderUI({
        req(cache())
        years <- cache()$survey_years

        if (is.null(years) || length(years) == 0) {
          return(NULL)
        }

        selectizeInput(
          ns("survey_start_year"),
          label = i18n$t("title_upload_survey_start_year"),
          choices = years,
          selected = cache()$start_survey_year
        )
      })
    }
  )
}
