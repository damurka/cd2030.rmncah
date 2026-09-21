familyPlanningUI <- function(id, i18n) {
  ns <- NS(id)

  countdownDashboard(
    dashboardId = ns('national_coverage'),
    dashboardTitle = i18n$t("title_fpet"),
    i18n = i18n,

    include_report = TRUE,

    box(
      title = i18n$t("title_fpet"),
      width = 12,
      downloadCoverageUI(ns('fpet'))
    )
  )
}

familyPlanningServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      fpet_data <- reactive({
        req(cache())
        cache()$fpet_data
      })

      downloadCoverageServer(
        id = 'fpet',
        filename = reactive('fpet'),
        data_fn = fpet_data,
        sheet_name = reactive(i18n$t("title_fpet")),
        plot_fun = function(d) {
  
          # Extract country to append to the translated base title
          country_name <- attr(d, "country") %||% ""
          base_title <- i18n$t("title_graph_fpet")
          
          plot(
            d, 
            title = paste0(base_title, ", ", country_name),
            x_axis = i18n$t("title_global_year"),
            y_axis = i18n$t("ylab_fpet"),
            caption = i18n$t("caption_fpet"),
            
            # Translate the legend (adjust the keys based on what they are named in your CSV/tibble)
            indicator_labels = list(
              prevalence = i18n$t("lbl_fpet_mcpr"),
              demand = i18n$t("lbl_fpet_demand")
            )
          )
        },
        i18n = i18n
      )

      countdownHeaderServer(
        'national_coverage',
        cache = cache,
        path = 'denominator-assessment',
        # section = 'sec-nat-cov-fp',
        i18n = i18n
      )
    }
  )
}