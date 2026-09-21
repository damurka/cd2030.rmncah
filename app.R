# increase the uploading file size limit to 2000M, now our upload is not just about hfd file, it also include the saved data.
options(shiny.maxRequestSize = 2 * 1024 * 1024^2)
options(future.globals.maxSize = 3 * 1024 * 1024^2) # 2 GB
options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)

# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

options(cd2030.selected_group = "rmncah")

library(officer)
library( officedown)
library(cd2030.core)

pacman::p_load(
  shiny,
  shinydashboard,
  shinycssloaders,
  shinyFiles,
  shinyjs,
  bslib,
  htmltools,
  dplyr,
  future,
  htmltools,
  openxlsx,
  plotly,
  purrr,
  promises,
  flextable,
  jsonlite,
  # forcats,
  lubridate,
  RColorBrewer,
  reactable,
  tidyr,
  markdown,
  rlang,
  # sf,
  shiny.i18n,
  stringr,
  waiter,
  webshot,
  update = FALSE
)

source("ui/content_body.R")
source("ui/content_dashboard.R")
source("ui/content_header.R")
source("ui/documentation_button.R")
source("ui/download/download_button.R")
source("ui/download/download_coverage.R")
source("ui/download/table_download.R")
source("ui/download/plot_download.R")
source("ui/download_report.R")
source("ui/input/admin-level-input.R")
source("ui/input/denominator-input.R")
source("ui/input/i18nSelectizeInput.R")
source("ui/input/indicator-select.R")
source("ui/input/population-select.R")
source("ui/help_button.R")
source("ui/message_box.R")
source("ui/render-plot.R")
source("ui/report_button.R")
source("ui/tab_panels.R")
source("ui/tooltips.R")

source("modules/introduction.R")

source("modules/0_upload_data.R")
source("modules/1a_checks_reporting_rate.R")
source("modules/1a_checks_outlier_detection.R")
source("modules/1a_data_completeness.R")
source("modules/1a_internal_consistency.R")
source("modules/1a_overall_score.R")

source("modules/1b_remove_years.R")

source("modules/1c_data_adjustment_changes.R")
source("modules/1c_data_adjustment.R")

source("modules/2_denominator_assessment.R")
source("modules/2_denominator_selection.R")

source("modules/3_bayesian.R")
source("modules/3_continuum_care.R")
source("modules/3_national_coverage.R")
source("modules/3_national_inequality.R")
source("modules/3_national_target.R")
source("modules/3_equity.R")

source("modules/4_subnational_coverage.R")
source("modules/4_subnational_inequality.R")
source("modules/4_subnational_target.R")

source('modules/5_mortality.R')
source('modules/5_mortality_completeness.R')
source('modules/5_mortality_mapping.R')

source('modules/6_utilization_dqa.R')
source('modules/6_national_service_utilization.R')
source('modules/6_subnational_service_utilization.R')
source('modules/6_mch_curative_index.R')

source('modules/7_health_system_national.R')
source('modules/7_health_system_subnational.R')
source('modules/7_health_system_comparison.R')
source('modules/7_private_sector.R')

app_name <- Sys.getenv("CDSUITE_SHINY_NAME", unset = "RMNCAH")
app_version <- Sys.getenv("CDSUITE_SHINY_VERSION", unset = "2.0.0")
selected_file <- Sys.getenv("CDSUITE_SHINY_SELECTED_FILE", unset = NA)
language <- Sys.getenv("CDSUITE_SHINY_LOCALE", unset = "en")

print(selected_file)

i18n <- init_i18n(translation_json_path = "translation/translation.json")
i18n$set_translation_language(language)

theme_bs3 <- bs_theme(version = 3, bootswatch = "flatly")

ui <- dashboardPage(
  skin = "green",
  title = app_name,
  header = dashboardHeader(
    title = HTML(paste0(app_name, "<sup>v", app_version, "</sup>")),
    uiOutput(
      outputId = "download_buttons",
      container = tags$li,
      class = "dropdown"
    )
  ),
  sidebar = dashboardSidebar(
    usei18n(i18n),
    selectizeInput(
      inputId = "selected_language",
      label = i18n$t("opt_global_change_language"),
      choices = c("English" = "en", "Français" = "fr", "Português" = "pt"),
      selected = language
    ),
    sidebarMenu(
      id = "tabs",
      menuItem(i18n$t("title_global_intro"), tabName = "introduction", icon = icon("info-circle")),
      menuItem(i18n$t("title_nav_load_data"), tabName = "upload_data", icon = icon("upload"), selected = TRUE),
      menuItem(i18n$t("title_nav_quality"),
        tabName = "quality_checks",
        icon = icon("check-circle"),
        startExpanded = TRUE,
        menuSubItem(i18n$t("title_rr_main"),
          tabName = "reporting_rate",
          icon = icon("chart-bar")
        ),
        menuSubItem(i18n$t("title_outlier_main"),
          tabName = "outlier_detection",
          icon = icon("exclamation-triangle")
        ),
        menuSubItem(i18n$t("title_complete_main"),
          tabName = "data_completeness",
          icon = icon("check-square")
        ),
        menuSubItem(i18n$t("title_consist_main"),
          tabName = "internal_consistency",
          icon = icon("tasks")
        ),
        menuSubItem(i18n$t("title_score_main"),
          tabName = "overall_score",
          icon = icon("star")
        )
      ),
      menuItem(i18n$t("btn_adjust_remove_years"), tabName = "remove_years", icon = icon("trash")),
      menuItem(i18n$t("title_adjust_main"),
        tabName = "data_adjustment_1",
        icon = icon("adjust"),
        menuSubItem(i18n$t("title_adjust_main"),
          tabName = "data_adjustment",
          icon = icon("adjust")
        ),
        menuSubItem(i18n$t("title_adjust_changes"),
          tabName = "data_adjustment_changes",
          icon = icon("adjust")
        )
      ),
      menuItem(i18n$t("title_denom_selection"),
        tabName = "denom_assess",
        icon = icon("calculator"),
        menuSubItem(i18n$t("title_denom_pop_trend"),
          tabName = "denominator_assessment",
          icon = icon("calculator")
        ),
        menuSubItem(i18n$t("title_denom_selection"),
          tabName = "denominator_selection",
          icon = icon("filter")
        )
      ),
      menuItem(i18n$t("title_nav_national_analysis"),
        tabName = "national_analysis",
        icon = icon("flag"),
        menuSubItem(i18n$t("title_coverage_national"),
          tabName = "national_coverage",
          icon = icon("chart-line")
        ),
        menuSubItem(i18n$t("title_nav_global_coverage"),
          tabName = "national_target",
          icon = icon("bullseye")
        ),
        menuSubItem(i18n$t('title_continuum'),
          tabName = 'continuum_care',
           icon = icon('heart-pulse')
        ),
        menuItem(
          i18n$t("title_inequ_national"), 
          icon = icon("scale-unbalanced"),
          startExpanded = TRUE,

          menuSubItem(i18n$t("title_routine_data"),
            tabName = "national_inequality",
            icon = icon("clipboard-list")
          ),
          menuSubItem(i18n$t("title_survey_data"),
            tabName = "equity_assessment",
            icon = icon("users")
          )
        )
      ),
      menuItem(i18n$t("title_nav_subnational_analysis"),
        tabName = "subnational_analysis",
        icon = icon("globe-africa"),
        menuSubItem(i18n$t("title_nav_subnational_coverage"),
          tabName = "subnational_coverage",
          icon = icon("map-marked")
        ),
        menuSubItem(i18n$t("title_inequ_subnational"),
          tabName = "subnational_inequality",
          icon = icon("balance-scale-right")
        ),
        menuSubItem(i18n$t("title_nav_global_coverage"),
          tabName = "subnational_target",
          icon = icon("user-slash")
        )
      ),
      menuItem(i18n$t('title_mortality'),
               tabName = 'mortality',
               icon = icon('balance-scale'),
               menuSubItem(i18n$t('title_mortality_institutional'),
                           tabName = 'mortality_institutional',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_mortality_mapping'),
                           tabName = 'mortality_mapping',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_mortality_completeness'),
                           tabName = 'mortality_completeness',
                           icon = icon('user-slash'))
      ),
      menuItem(i18n$t('title_service_utilization'),
               tabName = 'service_utilization',
               icon = icon('balance-scale'),
               menuSubItem(i18n$t('title_utilization_dqa'),
                           tabName = 'utilization_dqa',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_national_utilization'),
                           tabName = 'national_utilization',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_subnational_utilization'),
                           tabName = 'subnational_utilization',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_mch_curative'),
                           tabName = 'mch_curative_index',
                           icon = icon('balance-scale-right'))
      ),
      menuItem(i18n$t('opt_health_system_performance'),
               tabName = 'system_performance',
               icon = icon('globe-africa'),
               menuSubItem(i18n$t('title_national_health_system'),
                           tabName = 'health_system_national',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_subnational_health_system'),
                           tabName = 'health_system_subnational',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_health_system_comparison'),
                           tabName = 'health_system_comparison',
                           icon = icon('user-slash')),
               menuSubItem(i18n$t('title_private_sector'),
                           tabName = 'private_sector',
                           icon = icon('user-slash'))
      ),
      menuItem(
        text = i18n$t('title_bayesian_analysis'),
        icon = icon('chart-area'), 
        menuSubItem(
          text = i18n$t('title_nav_national_analysis'),
          tabName = 'bayesian_national',
          icon = icon('flag')
        ),
        
        menuSubItem(
          text = i18n$t('title_nav_subnational_analysis'),
          tabName = 'bayesian_subnational',
          icon = icon('map')
        )
      )
    )
  ),
  body = dashboardBody(
    theme = theme_bs3,
    useShinyjs(),
    useWaiter(),
    useHostess(),
    use_tooltips(),
    waiterShowOnLoad(
      color = "#ffffff",
      html = tagList(
        hostess_loader(
          "loader",
          preset = "bubble",
          text_color = "#7bc148",
          class = "label-center",
          center_page = TRUE,
          stroke_color = "#7bc148"
        ),
        br(),
        tagAppendAttributes(
          style = "margin-left: -75px",
          p(
            style = "color: #000000; font-weight: bold;",
            sample(
              c(
                "We are loading the app. Fetching stardust...",
                "The app is almost ready. Summoning unicorns...",
                "Hold on, the app is being loaded! Chasing rainbows...",
                "We are loading the app: teaching squirrels to water ski...",
                "App is loading! Counting clouds..."
              ),
              1
            )
          )
        )
      )
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-icons.css"),
      tags$script(src = "jquery.slimscroll.min.js"),
      tags$script(src = "header-brand.js"),
      tags$script(HTML("
        $(function() {
          $('body,html,.wrapper').css({ 'height':'auto', 'min-height':'100%' });
          $('body').addClass('fixed');
          var windowHeight  = $(window).height();
          var footerHeight  = $('.main-footer').outerHeight() || 0;
          $('.content-wrapper').css('min-height', windowHeight - footerHeight);
          if ($('.main-sidebar').find('slimScrollDiv').length === 0) {
            $('.sidebar').slimScroll({
              height: (windowHeight - $('.main-header').height()) + 'px'
            });
          }
        });
      "))
    ),
    tabItems(
      tabItem(tabName = "introduction", introductionUI("introduction", i18n = i18n)),
      tabItem(tabName = "upload_data", uploadDataUI("upload_data", i18n = i18n, is_electron = !is.na(selected_file))),
      tabItem(tabName = "reporting_rate", reportingRateUI("reporting_rate", i18n = i18n)),
      tabItem(tabName = "data_completeness", dataCompletenessUI("data_completeness", i18n = i18n)),
      tabItem(tabName = "internal_consistency", internalConsistencyUI("internal_consistency", i18n = i18n)),
      tabItem(tabName = "outlier_detection", outlierDetectionUI("outlier_detection", i18n = i18n)),
      tabItem(tabName = "overall_score", overallScoreUI("overall_score", i18n = i18n)),
      tabItem(tabName = "remove_years", removeYearsUI("remove_years", i18n = i18n)),
      tabItem(tabName = "data_adjustment", dataAjustmentUI("data_adjustment", i18n = i18n)),
      tabItem(tabName = "data_adjustment_changes", adjustmentChangesUI("data_adjustment_changes", i18n = i18n)),
      tabItem(tabName = "denominator_assessment", denominatorAssessmentUI("denominator_assessment", i18n = i18n)),
      tabItem(tabName = "denominator_selection", denominatorSelectionUI("denominator_selection", i18n = i18n)),
      tabItem(tabName = "national_coverage", nationalCoverageUI("national_coverage", i18n = i18n)),
      tabItem(tabName = "continuum_care", continuumCoverageUI("continuum_care", i18n = i18n)),
      tabItem(tabName = "subnational_coverage", subnationalCoverageUI("subnational_coverage", i18n = i18n)),
      tabItem(tabName = "national_inequality", nationalInequalityUI("national_inequality", i18n = i18n)),
      tabItem(tabName = "subnational_inequality", subnationalInequalityUI("subnational_inequality", i18n = i18n)),
      tabItem(tabName = "national_target", nationalTargetUI("national_target", i18n = i18n)),
      tabItem(tabName = "subnational_target", subnationalTargetUI("subnational_target", i18n = i18n)),
      tabItem(tabName = "equity_assessment", equityUI("equity_assessment", i18n = i18n)),
      tabItem(tabName = 'mortality_institutional', mortalityUI('mortality_institutional', i18n = i18n)),
      tabItem(tabName = 'mortality_mapping', mortalityMappingUI('mortality_mapping', i18n = i18n)),
      tabItem(tabName = 'mortality_completeness', mortalityCompletenessUI('mortality_completeness', i18n = i18n)),
      tabItem(tabName = 'utilization_dqa', utilizationDqaUI('utilization_dqa', i18n = i18n)),
      tabItem(tabName = 'national_utilization', nationalServiceUtilizationUI('national_utilization', i18n = i18n)),
      tabItem(tabName = 'subnational_utilization', subnationalServiceUtilizationUI('subnational_utilization', i18n = i18n)),
      tabItem(tabName = 'mch_curative_index', mchCurativeIndexUI('mch_curative_index', i18n = i18n)),
      tabItem(tabName = 'health_system_national', healthSystemNationalUI('health_system_national', i18n = i18n)),
      tabItem(tabName = 'health_system_subnational', healthSystemSubnationalUI('health_system_subnational', i18n = i18n)),
      tabItem(tabName = 'health_system_comparison', healthSystemComparisonUI('health_system_comparison', i18n = i18n)),
      # tabItem(tabName = 'fpet_projection', familyPlanningUI('fpet_projection', i18n = i18n)),
      tabItem(tabName = 'private_sector', privateSectorUI('private_sector', i18n = i18n)),
      tabItem(tabName = 'bayesian_national', bayesianUI('bayesian_national', i18n = i18n)),
      tabItem(tabName = 'bayesian_subnational', bayesianUI('bayesian_subnational', i18n = i18n))
    )
  )
)

server <- function(input, output, session) {
  hostess <- Hostess$new("loader", infinite = TRUE)
  hostess$start()

  introductionServer("introduction", selected_language = reactive(input$selected_language))
  cache <- uploadDataServer("upload_data", i18n, selected_file)
  observeEvent(c(cache(), cache()$language), {
    req(cache())

    update_lang(cache()$language)
    updateHeader(cache()$country, i18n)
  })

  observeEvent(input$selected_language, {
    if (!isTruthy(cache())) {
      update_lang(input$selected_language)
      updateSelectizeInput(session, input$selected_language)
    } else {
      cache()$set_language(input$selected_language)
      updateSelectizeInput(session, cache()$language)
    }
    session$sendCustomMessage("reinit-tooltips", TRUE)
  })

  reportingRateServer("reporting_rate", cache, i18n)
  dataCompletenessServer("data_completeness", cache, i18n)
  internalConsistencyServer("internal_consistency", cache, i18n)
  outlierDetectionServer("outlier_detection", cache, i18n)
  overallScoreServer("overall_score", cache, i18n)
  removeYearsServer("remove_years", cache, i18n)
  dataAdjustmentServer("data_adjustment", cache, i18n)
  adjustmentChangesServer("data_adjustment_changes", cache, i18n)
  denominatorAssessmentServer("denominator_assessment", cache, i18n)
  denominatorSelectionServer("denominator_selection", cache, i18n)
  nationalCoverageServer("national_coverage", cache, i18n)
  subnationalCoverageServer("subnational_coverage", cache, i18n)
  nationalInequalityServer("national_inequality", cache, i18n)
  subnationalInequalityServer("subnational_inequality", cache, i18n)
  nationalTargetServer("national_target", cache, i18n)
  continuumCoverageServer("continuum_care", cache, i18n)
  subnationalTargetServer("subnational_target", cache, i18n)
  equityServer("equity_assessment", cache, i18n)
  mortalityServer('mortality_institutional', cache, i18n)
  mortalityMappingServer('mortality_mapping', cache, i18n)
  mortalityCompletenessServer('mortality_completeness', cache, i18n)
  utilizationDqaServer('utilization_dqa', cache, i18n)
  nationalServiceUtilizationServer('national_utilization', cache, i18n)
  subnationalServiceUtilizationServer('subnational_utilization', cache, i18n)
  mchCurativeIndexServer('mch_curative_index', cache, i18n)
  healthSystemNationalServer('health_system_national', cache, i18n)
  healthSystemSubnationalServer('health_system_subnational', cache, i18n)
  healthSystemComparisonServer('health_system_comparison', cache, i18n)
  privateSectorServer('private_sector', cache, i18n)
  bayesianServer('bayesian_national', cache, i18n, 'national')
  bayesianServer('bayesian_subnational', cache, i18n, 'adminlevel_1')
  # familyPlanningServer('fpet_projection', cache, i18n)
  downloadReportServer("download_report", cache, i18n)

  # session$onSessionEnded(stopApp)

  onFlushed(function() {
    hostess$close()
    waiter_hide()
  }, once = TRUE)

  updateHeader <- function(country, i18n) {
    session$sendCustomMessage(
      "setHeaderBrand",
      list(html = str_glue("{country} &mdash; {i18n$t('title_nav_countdown')}"))
    )
  }

  output$download_buttons <- renderUI({
    req(cache())
    downloadReportUI("download_report", i18n)
  })
}

shinyApp(ui = ui, server = server)
