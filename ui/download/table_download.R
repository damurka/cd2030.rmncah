tableDownloadsUI <- function(id, i18n, title_key, control_type = c("year", "indicator"), width = 6) {
  ns <- NS(id)
  control_type <- arg_match(control_type)
  
  left_control_ui <- if (control_type == "year") {
    selectizeInput(ns("control_input"), label = i18n$t("title_global_year"), choices = NULL)
  } else if (control_type == "indicator") {
    indicatorSelect(id = ns("control_input"), i18n = i18n)
  } else {
    NULL
  }

  box(
    title = i18n$t(title_key),
    width = width,
    status = "success",
    fluidRow(
      column(3, left_control_ui),
      column(3, offset = 6, downloadButtonUI(ns("download_data"))),
      column(12, withSpinner(reactableOutput(ns("table"))))
    )
  )
}

tableDownloadsServer <- function(
    id,
    cache,
    i18n,
    data,
    columns = NULL,
    control_type = c("year", "indicator"),
    filename = reactive("download"),
    label_key = "btn_global_download_data",
    extension = reactive("xlsx"),
    data_transform = NULL,
    excel_write_fun = NULL
) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(filename))
  stopifnot(is.reactive(extension))
  
  control_type <- arg_match(control_type)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      
      selected_value <- if (control_type == "year") {
        
        # Automatically update the year dropdown choices from cache
        observe({
          req(cache(), cache()$data_years)
          updateSelectizeInput(session, "control_input", choices = cache()$data_years)
        })
        
        # Return the standard input
        reactive({ input$control_input })
        
      } else if (control_type == "indicator") {
        
        # Call your custom server module to get the selected indicator
        indicatorSelectServer("control_input")
        
      } else {
        reactive({ NULL })
      }
      
      output$table <- renderReactable({
        req(data())

        dt <- if (!is.null(data_transform) && is.function(data_transform)) {
          data() %>% data_transform()
        } else {
          data()
        }
        
        if (control_type == "year" && "year" %in% names(dt)) {
          dt <- dt %>% 
            filter(year == as.integer(selected_value()))
        }
        
        base_cols <- list(
          year = colDef(
            name = i18n$t("title_global_year"),
            aggregate = "unique" 
          ),
          adminlevel_1 = colDef(
            name = i18n$t("opt_adminlevel_1")
          ),
          district = colDef(
            name = i18n$t("opt_district")
          ),
          month = colDef(
            name = i18n$t("lbl_axis_x_outlier_trend")
          )
        )
        
        # ---------------------------------------------------------
        # THE MAGIC: Auto-detecting & Auto-translating Columns
        # ---------------------------------------------------------
        
        # A) Auto-detect "cov_" columns
        cov_cols <- grep("^cov_", names(dt), value = TRUE)
        if (length(cov_cols) > 0) {
          cov_defs <- set_names(lapply(cov_cols, function(c) {
            indicator <- i18n$t(paste0("opt_", str_remove(str_remove(c, "_[^_]*$"), "^cov_")))
            colDef(name = as.character(str_glue(i18n$t("lbl_axis_y_coverage"))))
          }), cov_cols)
          base_cols <- utils::modifyList(base_cols, cov_defs)
        }
        
        # B) Auto-detect "_med" and "_mad" columns (Extreme Outliers Table)
        med_cols <- grep("_med$", names(dt), value = TRUE)
        if (length(med_cols) > 0) {
          
          # Median columns (with str_glue interpolation)
          med_defs <- set_names(lapply(med_cols, function(x) {
            base_ind <- sub("_med$", "", x)
            indicator <- i18n$t(paste0("opt_", base_ind)) # This creates the '{indicator}' variable for str_glue
            colDef(name = as.character(str_glue(i18n$t("opt_median"))))
          }), med_cols)
          base_cols <- utils::modifyList(base_cols, med_defs)
          
          # MAD columns (with str_glue interpolation)
          mad_cols <- grep("_mad$", names(dt), value = TRUE)
          mad_defs <- set_names(lapply(mad_cols, function(x) {
            base_ind <- sub("_mad$", "", x)
            indicator <- i18n$t(paste0("opt_", base_ind)) # This creates the '{indicator}' variable for str_glue
            colDef(name = as.character(str_glue(i18n$t("opt_mad"))))
          }), mad_cols)
          base_cols <- utils::modifyList(base_cols, mad_defs)
          
          # The Base Indicator column itself
          base_inds <- sub("_med$", "", med_cols)
          ind_defs <- set_names(lapply(base_inds, function(x) colDef(name = i18n$t(paste0("opt_", x)))), base_inds)
          base_cols <- utils::modifyList(base_cols, ind_defs)
        }
        
        # C) If it's pure indicator selection, translate the standalone indicator column
        if (control_type == "indicator") {
          ind_val <- selected_value()
          if (ind_val %in% names(dt)) {
            base_cols[[ind_val]] <- colDef(name = i18n$t(paste0("opt_", ind_val)))
          }
        }
        
        final_cols <- if (is.null(columns)) base_cols else modifyList(base_cols, columns)
        final_cols <- final_cols[names(final_cols) %in% names(dt)]

        dt %>%
          reactable(
            columns = final_cols,
            defaultColDef = colDef(
              cell = function(value) {
                if (!is.numeric(value)) {
                  return(value)
                }
                format(round(value), nsmall = 0)
              }
            )
          )
      })

      if (!is.null(excel_write_fun) && is.function(excel_write_fun)) {
        downloadButtonServer(
          id = "download_data",
          filename = reactive(paste0(filename(), "_", selected_value())),
          extension = extension,
          data = data,
          i18n = i18n,
          label = label_key,
          icon = "table",
          button_class = "btn-data",
          content = function(file, d) {
            wb <- createWorkbook()
            excel_write_fun(wb, d) # IMPORTANT: d is already evaluated data (not reactive)
            saveWorkbook(wb, file, overwrite = TRUE)
          }
        )
      }
      
      return(selected_value)
    }
  )
}
