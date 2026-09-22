rr_indicators <- c("heat_map", "bar")

reportingRateUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("reporting_rate"),
    dashboardTitle = i18n$t("title_rr_main"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, indicatorSelect(ns("indicator"), i18n,
        tooltip = "tt_rr_indicator",
        indicators = c("opt_anc" = "anc_rr", "opt_idelv" = "idelv_rr", "opt_vacc" = "vacc_rr", "opt_opd" = "opd_rr")
      )),
      column(3, cdChipNumber(ns("threshold"), "title_rr_threshold", i18n, value = 90, min = 0, max = 100, unit = "%", picks = c(70, 80, 90, 95), default = 90)),
      column(6, adminLevelInputUI(ns("admin_level"), i18n)),
    ),
    tabPanelsUI(ns("panel"), i18n, "title_rr_subnational", downloadCoverageUI, indicators = rr_indicators, showCustom = FALSE),
    box(
      title = uiOutput(ns("district_rr_title")),
      status = "success",
      collapsible = TRUE,
      width = 6,
      plotDownloadsRowUI(ns("rr_national"))
    ),
    tableDownloadsUI(ns("low_reporting"), i18n, "title_rr_low_reporting")
  )
}

reportingRateServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      state <- reactiveValues(loaded = FALSE)
      indicator_val <- indicatorSelectServer("indicator")
      admin <- adminLevelInputServer("admin_level", cache, i18n, allow_select_all = TRUE, show_district = FALSE)

      admin_level <- reactive({
        req(admin())
        admin()$admin_level
      })

      region <- reactive({
        req(admin())
        admin()$region
      })

      threshold <- reactive({
        req(cache())
        cache()$performance_threshold
      })

      reporting_rate <- reactive({
        req(cache(), admin_level())

        rate <- if (admin_level() == "adminlevel_1" && is.null(region())) {
          cache()$reporting_rate_admin1
        } else if (admin_level() == "district" && is.null(region())) {
          cache()$reporting_rate_district
        } else if (admin_level() == "adminlevel_1" && !is.null(region())) {
          cache()$calculate_reporting_rate(admin_level(), region())
        } else {
          NULL
        }
        return(rate)
      })

      subnational_rr <- reactive({
        req(reporting_rate(), indicator_val())

        reporting_rate() %>%
          select(any_of(c("adminlevel_1", "district", "year", indicator_val())))
      })

      district_rr <- reactive({
        req(cache())
        if (is.null(region())) {
          return(cache()$district_reporting_rate)
        }
        cache()$calculate_district_reporting_rate(region())
      })

      # Now we define the data. It safely uses `selected_year()` because 
      # the reactive won't fire until the user actually makes a selection!
      district_low_rr <- reactive({
        req(subnational_rr(), indicator_val())

        subnational_rr() %>%
          filter(!!sym(indicator_val()) < threshold())
      })

      # show the stored threshold once the data is there and the chip exists
      threshold_mounted <- cdMounted(input, "threshold")
      threshold_shown <- FALSE
      observeEvent(list(data(), threshold_mounted()), {
        req(data(), threshold_mounted())
        if (threshold_shown) return()
        threshold_shown <<- TRUE
        updateCdChip("threshold", session, value = threshold())
      })

      observeEvent(input$threshold, {
        req(cache())
        cache()$set_performance_threshold(as.integer(input$threshold))
      })

      output$district_rr_title <- renderUI({
        if (is.null(region())) {
          i18n$t("title_rr_national")
        } else {
          region_name <- region()
          str_glue(i18n$t("title_rr_region"))
        }
      })
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0("rr_", current_indicator, "_plot")),
            data_fn = subnational_rr,
            sheet_name = reactive(i18n$t("title_rr_average")),
            plot_fun = function(d) {
              req(indicator_val(), threshold(), admin_level())
              
              region_name <- region()
              admin_level <- i18n$t(if (is.null(region_name)) paste0("opt_", admin_level()) else "opt_district")
              indicator <- i18n$t(paste0("opt_", str_remove(indicator_val(), "_rr")))
              plot(d,
                   plot_type = current_indicator,
                   indicator = indicator_val(),
                   threshold = threshold(),
                   title = str_glue(i18n$t(if (is.null(region_name)) "plt_title_rr_heatmap" else "plt_title_rr_heatmap_region")),
                   x_axis = if (current_indicator == "bar") i18n$t("title_global_year")  else admin_level,
                   y_axis = if (current_indicator == "bar") i18n$t("title_rr_main") else i18n$t("title_global_year"),
                   legend = str_glue(i18n$t("lbl_leg_rr_heatmap"))
              )
            },
            i18n = i18n
          )
        },
        indicators = rr_indicators
      )

      plotDownloadsRowServer(
        id = "rr_national",
        i18n = i18n,
        plot_data = district_rr,
        plot_filename = reactive("rr_national_plot"),
        plot_fun = function(d) {
          reporting_rate <- threshold()
          plot(
            district_rr(),
            title = str_glue(i18n$t("plt_title_rr_national")),
            caption = str_glue(i18n$t("plt_caption_rr_national")),
            indicator_labels = c(
              anc = i18n$t("opt_anc"),
              idelv = i18n$t("opt_idelv"),
              vacc = i18n$t("opt_vacc")
            )
          )
        },
        excel_write_fun = function(wb, d) {
          sheet_name_2 <- str_glue(i18n$t("lbl_sheet_rr_district"))
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = str_glue(i18n$t("tab_rr_district")), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = d, startCol = 1, startRow = 3)
        }
      )
      
      tableDownloadsServer(
        "low_reporting",
        cache,
        i18n,
        control_type = "year",
        data = district_low_rr,
        columns = c(
          set_names(
            list(
              colDef(
                name = i18n$t(
                  paste0("opt_", str_remove(indicator_val(), "_rr"))
                )
              )
            ),
            indicator_val()
          )
        ),
        filename = reactive("district_low_reporting_rate"),
        excel_write_fun = function(wb, data) {
          sheet_name_1 <- i18n$t("title_rr_district_low")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t("tab_rr_district_year")), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = data, startCol = 1, startRow = 3)
        }
      )

      countdownHeaderServer(
        "reporting_rate",
        cache = cache,
        path = "2-data-quality-assessment",
        section = "reporting-completeness",
        i18n = i18n
      )
    }
  )
}
