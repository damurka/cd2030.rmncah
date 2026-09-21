equity_indicators <- c("penta1", "penta3", "measles1")

equityUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns("national_inequality"),
    dashboardTitle = i18n$t("title_nav_equity"),
    i18n = i18n,
    countdownOptions(
      title = i18n$t("title_global_options"),
      column(3, i18nSelectizeInput(ns("type"),
        label = i18n$t("title_equity_type"),
        choices = c(
          "opt_equity_area" = "area",
          "opt_equity_meduc" = "meduc",
          "opt_equity_wiq" = "wiq"
        )
      ))
    ),
    include_report = TRUE,
    tabPanelsUI(ns("panel"), i18n, "title_equity_analysis", downloadCoverageUI,
      indicators = equity_indicators
    )
  )
}

equityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      indicator <- indicatorSelectServer("indicator")

      data <- reactive({
        req(cache())
        switch(input$type,
          "area" = cache()$area_survey,
          "meduc" = cache()$education_survey,
          "wiq" = cache()$wiq_survey
        )
      })

      tabPanelsServer(
        "panel",
        serverInput = function(id, current_indicator) {
          downloadCoverageServer(
            id = id, # or just ind if inside the same module id
            filename = reactive(paste0(current_indicator, "_", input$type, "_equity")),
            data_fn = data,
            sheet_name = reactive(i18n$t(paste0(current_indicator, "_derived_coverage"))),
            plot_fun = function(d) {
              req(input$type)
              indicator <- i18n$t(paste0("opt_", current_indicator))
              switch(input$type,
                area = equiplot_area(d, 
                                     current_indicator,
                                     title = str_glue(i18n$t("plt_title_equity_area")),
                                     x_title = str_glue(i18n$t("lbl_axis_y_coverage")),
                                     legend_title = i18n$t("lbl_leg_equity_area"),
                                     legend_labels = list(
                                       "Rural" = i18n$t("lbl_equity_area_rural"),
                                       "Urban" = i18n$t("lbl_equity_area_urban")
                                     )
                                    ),
                meduc = equiplot_education(d, 
                                           current_indicator,
                                           title = str_glue(i18n$t("plt_title_equity_education")),
                                           x_title = str_glue(i18n$t("lbl_axis_y_coverage")),
                                           legend_title = i18n$t("lbl_leg_equity_education"),
                                           legend_labels = list(
                                             "none" ~ i18n$t("lbl_equity_edu_none"),
                                             "primary" ~ i18n$t("lbl_equity_edu_primary"),
                                             "secondary+" ~ i18n$t("lbl_equity_edu_secondary")
                                           )
                                          ),
                wiq = equiplot_wealth(d, 
                                      current_indicator,
                                      title = str_glue(i18n$t("plt_title_equity_wealth")),
                                      x_title = str_glue(i18n$t("lbl_axis_y_coverage")),
                                      legend_title = i18n$t("lbl_leg_equity_wealth")
                                    )
              )
            },
            i18n = i18n
          )
        },
        indicators = equity_indicators
      )

      countdownHeaderServer(
        "national_inequality",
        cache = cache,
        path = "national-inequality",
        section = "interpretation-of-equiplots",
        i18n = i18n
      )
    }
  )
}
