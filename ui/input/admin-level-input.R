# Admin level and region, as filter chips. The level chip is always there; the region chip appears for a level that
# has areas to choose from, and its options are the areas (districts are listed under their admin 1).
# The returned reactive gives list(admin_level = , region = ), where region is a single name or NULL for "all".

admin_level_choices <- function(include_national = FALSE) {
  c(
    if (include_national) c("opt_national" = "national") else NULL,
    "opt_adminlevel_1" = "adminlevel_1",
    "opt_district" = "district"
  )
}

adminLevelInputUI <- function(id, i18n, include_national = FALSE, show_admin_level = TRUE) {
  ns <- NS(id)
  choices <- admin_level_choices(include_national)

  tagList(
    if (show_admin_level) {
      cdChipSelect(ns("admin"), "title_global_admin_level", choices, i18n, hint = "tt_global_admin_level",
                   default = unname(choices)[[1]])
    },
    shiny.react::reactOutput(ns("region_ui"))
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

      # every area at this level, grouped by admin 1 for districts
      region_options <- reactive({
        req(cache())
        if (!region_is_visible()) return(NULL)

        admin_col <- admin_val()
        is_district <- admin_col == "district"

        region_data <- cache()$subnational_regions %>%
          filter(if (is_district && !is.null(selected_admin1())) adminlevel_1 == selected_admin1() else TRUE) %>%
          distinct(!!sym(admin_col), .keep_all = TRUE) %>%
          # districts are listed under their parent, so sort by parent first or each heading repeats
          arrange(if (is_district) adminlevel_1 else !!sym(admin_col), !!sym(admin_col))

        opts <- cdPlainOptions(region_data[[admin_col]], if (is_district) region_data$adminlevel_1 else NULL)

        if (allow_select_all) {
          opts <- c(list(list(key = "_all_", text = cdText(i18n, "opt_global_select_all"))), opts)
        }
        opts
      })

      output$region_ui <- shiny.react::renderReact({
        opts <- region_options()
        if (is.null(opts)) return(NULL)

        admin_col <- admin_val()
        # `key` remounts the chip when the level changes, so a region from the old level is never kept
        cdChipSelect(ns("region"), paste0("opt_", admin_col), i18n = i18n, options = opts,
                     selected = if (allow_select_all) "_all_" else "", key = admin_col)
      })

      reactive({
        if (region_is_visible()) {
          req(!is.null(input$region), nzchar(input$region))
        }
        list(
          admin_level = admin_val(),
          region = region()
        )
      })
    }
  )
}
