adminLevelInputUI <- function(id, i18n, include_national = FALSE, show_admin_level = TRUE) {
  ns <- NS(id)

  choices <- c(
    if (include_national) c("opt_national" = "national") else NULL,
    "opt_adminlevel_1" = "adminlevel_1",
    "opt_district" = "district"
  )

  fluidRow(
    if (show_admin_level) {
      column(
        6,
        i18nSelectizeInput(
          ns("admin"),
          label = "title_global_admin_level",
          tooltip = "tt_global_admin_level",
          choices = choices
        )
      )
    },
    column(if (show_admin_level) 6 else 12, uiOutput(ns("region_ui")))
  )
}

adminLevelInputServer <- function(id, cache, i18n, allow_select_all = FALSE, show_district = TRUE, show_region = TRUE, show_admin_level = TRUE, selected_admin1 = reactive(NULL)) {
  stopifnot(is.reactive(cache))
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      admin_val <- reactive({
        if (show_admin_level) {
          req(input$admin)
          input$admin
        } else if (!show_district) {
          "adminlevel_1"
        } else {
          "district"
        }
      })
      
      region_is_visible <- reactive({
        current_admin <- admin_val()
        
        # 1. Determine if the Region input is currently visible
        #    (Adjust 'show_district' logic based on your specific needs)
        (current_admin != "national") &&
          show_region &&
          (current_admin != "district" || show_district)
      })

      observeEvent(admin_val(), {
        freezeReactiveValue(input, "region")
      })

      region <- reactive({

        if (!region_is_visible() || is.null(input$region) || input$region == "_all_" || !nzchar(input$region)) {
          return(NULL)
        }

        input$region
      })

      region_choices <- reactive({
        req(cache())

        if (!region_is_visible()) {
          return()
        }

        admin_col <- admin_val()
        is_district <- admin_col == "district"

        region_data <- cache()$subnational_regions %>%
          filter(if (admin_col == "district" && !is.null(selected_admin1())) adminlevel_1 == selected_admin1() else TRUE) %>%
          distinct(!!sym(admin_col), .keep_all = TRUE) %>%
          arrange(!!sym(admin_col))

        choices_list <- region_data %>% pull(!!sym(admin_col))
        names(choices_list) <- choices_list

        if (is_district) {
          choices_list <- split(choices_list, region_data$adminlevel_1)
        }

        if (allow_select_all) {
          all_choice <- set_names("_all_", i18n$t("opt_global_select_all"))
          if (is_district) {
            choices_list <- c(list(" " = all_choice), choices_list)
          } else {
            choices_list <- c(all_choice, choices_list)
          }
        }

        return(choices_list)
      })

      output$region_ui <- renderUI({
        choices <- region_choices()

        # If the reactive returned NULL, don't draw anything
        if (is.null(choices)) {
          return()
        }

        admin_col <- admin_val()
        new_label <- i18n$t(paste0("opt_", admin_col))

        selectizeInput(
          inputId = ns("region"),
          label = new_label,
          choices = choices,
          selected = if (allow_select_all) "_all_" else NULL,
          options = list(placeholder = "msg_global_select_region")
        )
      })

      reactive({
        if (region_is_visible()) {
          req(!is.null(input$region))
        }
        list(
          admin_level = admin_val(),
          region = region()
        )
      })
    }
  )
}
