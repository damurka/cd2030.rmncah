target_indicators <- c('anc4', 'instlivebirths', 'vaccine')

targetUI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    tabPanelsUI(ns("panel"), i18n, "title_nav_global_coverage", downloadCoverageUI, 
                indicators = target_indicators, showCustom = FALSE),
    uiOutput(ns("dynamic_table_downloads_ui"))
  )
}

targetServer <- function(id, cache, i18n, admin_level, region = reactive(NULL)) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(admin_level))
  stopifnot(is.reactive(region))
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      # 1. Dynamically render the Table Downloads UI based on admin_level()
      output$dynamic_table_downloads_ui <- renderUI({
        req(admin_level())
        
        title_key <- if (admin_level() == "adminlevel_1") {
          "title_target_admin1_rate"
        } else {
          "title_target_district_rate"
        }
        
        tableDownloadsUI(
          ns("district_low_reporting"), 
          i18n, 
          title_key, 
          control_type = "indicator"
        )
      })
      
      # 2. Server logic for the table downloads module
      selected_indicator <- tableDownloadsServer(
        "district_low_reporting",
        cache,
        i18n,
        control_type = "indicator",
        data = reactive(district_coverage_rate()),
        filename = reactive(paste0(admin_level(), "_high_coverage_rate")), 
        excel_write_fun = function(wb, d) { 
          
          sheet_title_key <- if (admin_level() == "adminlevel_1") {
            "title_target_admin1_rate"
          } else {
            "title_target_district_rate"
          }
          
          sheet_name_1 <- i18n$t(sheet_title_key)
          
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = sheet_name_1, startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = d, startCol = 1, startRow = 3) 
        }
      )

      district_coverage_rate <- reactive({
        req(cache(), cache()$check_inequality_params, selected_indicator())
        
        # FIXED: Translate query logic for get_high_performers to avoid get_admin_columns crash
        query_admin <- if (!is.null(region())) "adminlevel_1" else admin_level()
        
        cache()$get_high_performers(
          indicator = selected_indicator(),
          admin_level = query_admin,
          region = region()
        )
      })
      
      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          denominator <- reactive({
            req(cache())
            cache()$get_denominator(current_indicator)
          })
          
          data_rx <- reactive({
            req(cache(), cache()$check_inequality_params)
            
            cache()$get_filtered_threshold(
              indicator = current_indicator, 
              target_unit = admin_level(), 
              region = region()
            )
          })
          
          downloadCoverageServer(
            id = id, 
            filename = reactive(paste0(current_indicator, "_global_target_", denominator())),
            data_fn = data_rx,
            sheet_name = reactive(i18n$t(paste0("opt_", current_indicator))),
            plot_fun = function(d) {
              
              title_key <- if (is.null(region())) "plt_title_target_national" else "plt_title_target_region"
              translated_x_labels <- list(
                bcg = i18n$t("opt_bcg"),
                penta3 = i18n$t("opt_penta3"),
                measles1 = i18n$t("opt_measles1"),
                dropout_penta13 = i18n$t("opt_dropout_penta13"),
                dropout_penta3mcv1 = i18n$t("opt_dropout_penta3mcv1")
              )
              
              admin_lvl_translated <- i18n$t(paste0("opt_", admin_level()))
              rate <- i18n$t(if (current_indicator == 'opt_dropout') 'opt_rate' else 'opt_coverage')
              sign <- if (current_indicator == 'opt_dropout') '<' else '≥'
              group <- i18n$t(paste0("opt_", current_indicator))
              coverage <- attr(d, "threshold")
              
              # FIXED: Explicitly pass named variables into str_glue so it overrides the reactive functions in the environment
              plot_title <- str_glue(
                i18n$t(title_key),
                admin_level = admin_lvl_translated,
                region = region() %||% "",
                coverage = coverage,
                group = group,
                rate = rate,
                sign = sign
              )
              
              plot(d,
                   title = plot_title,
                   y_axis = i18n$t("lbl_axis_y_pct_subnat"),
                   legend_title = i18n$t("title_global_year"),
                   x_labels = translated_x_labels
              )
            },
            i18n = i18n
          )
        },
        indicators = target_indicators
      )
    }
  )
}