# The page registry: every analysis page in one place. Each entry says what the page is called, where it sits, what it
# is about, which help chapter it opens, and which module builds it. app.R builds the page containers and starts
# the servers from this list (cd_pages_ui(), cd_pages_server()), and cd_page_ui() reads the title/section/subtitle
# from here, so a page module never repeats them. To add a page: write its module, then add one entry here and
# a nav item in app.R.

cd_page_registry <- list(
  cd_page_def(
    id = "reporting_rate",
    ui = reporting_rate_ui,
    server = reporting_rate_server,
    title = "title_rr_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_rr_main",
    help = c("2-data-quality-assessment", "reporting-completeness")
  ),
  cd_page_def(
    id = "data_completeness",
    ui = data_completeness_ui,
    server = data_completeness_server,
    title = "title_complete_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_complete_main",
    help = c("2-data-quality-assessment", "data-missingness")
  ),
  cd_page_def(
    id = "internal_consistency",
    ui = internal_consistency_ui,
    server = internal_consistency_server,
    title = "title_consist_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_consist_main",
    help = c("2-data-quality-assessment", "ratio-calculations")
  ),
  cd_page_def(
    id = "outlier_detection",
    ui = outlier_detection_ui,
    server = outlier_detection_server,
    title = "title_outlier_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_outlier_main",
    help = c("2-data-quality-assessment", "outlier-detection")
  ),
  cd_page_def(
    id = "overall_score",
    ui = overall_score_ui,
    server = overall_score_server,
    title = "title_score_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_score_main",
    help = c("2-data-quality-assessment", "overall-quality-score"),
    report = "data_quality"
  ),
  cd_page_def(
    id = "remove_years",
    ui = remove_years_ui,
    server = remove_years_server,
    title = "btn_adjust_remove_years",
    section = "lbl_nav_section_quality",
    subtitle = "sub_remove_years",
    help = c("3-data-adjustment", "631-remove-years"),
    active = FALSE
  ),
  cd_page_def(
    id = "data_adjustment",
    ui = data_adjustment_ui,
    server = data_adjustment_server,
    title = "title_adjust_main",
    section = "lbl_nav_section_quality",
    subtitle = "sub_adjust_main",
    help = c("3-data-adjustment"),
    active = FALSE
  ),
  cd_page_def(
    id = "data_adjustment_changes",
    ui = adjustment_changes_ui,
    server = adjustment_changes_server,
    title = "title_adjust_changes",
    section = "lbl_nav_section_quality",
    subtitle = "sub_adjust_changes",
    help = c("3-data-adjustment"),
    report = "adjustment"
  ),
  cd_page_def(
    id = "denominator_assessment",
    ui = denominator_assessment_ui,
    server = denominator_assessment_server,
    title = "title_denom_assessment",
    section = "lbl_nav_section_denominators",
    subtitle = "sub_denom_assessment",
    help = c("4-denominator-selection", "population-trend-comparison")
  ),
  cd_page_def(
    id = "denominator_selection",
    ui = denominator_selection_ui,
    server = denominator_selection_server,
    title = "title_denom_selection",
    section = "lbl_nav_section_denominators",
    subtitle = "sub_denom_selection",
    help = c("4-denominator-selection"),
    report = "denominator_selection"
  ),
  cd_page_def(
    id = "national_coverage",
    ui = national_coverage_ui,
    server = national_coverage_server,
    title = "title_coverage_national",
    section = "title_nav_national_analysis",
    subtitle = "sub_cov_national",
    help = c("5-coverage-estimation"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "continuum_care",
    ui = continuum_coverage_ui,
    server = continuum_coverage_server,
    title = "title_continuum",
    section = "title_nav_national_analysis",
    subtitle = "sub_continuum",
    help = c("5-coverage-estimation"),
    denominator = TRUE,
    report = "national_coverage"
  ),
  cd_page_def(
    id = "subnational_coverage",
    ui = subnational_coverage_ui,
    server = subnational_coverage_server,
    title = "title_nav_subnational_coverage",
    section = "title_nav_subnational_analysis",
    subtitle = "sub_cov_subnational",
    help = c("7-subnational-analysis"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "national_inequality",
    ui = national_inequality_ui,
    server = national_inequality_server,
    title = "title_inequ_national",
    section = "title_nav_national_analysis",
    subtitle = "sub_inequ_national",
    help = c("6-equity-analysis"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "subnational_inequality",
    ui = subnational_inequality_ui,
    server = subnational_inequality_server,
    title = "title_inequ_subnational",
    section = "title_nav_subnational_analysis",
    subtitle = "sub_inequ_subnational",
    help = c("7-subnational-analysis"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "national_target",
    ui = national_target_ui,
    server = national_target_server,
    title = "title_nav_global_coverage",
    section = "title_nav_national_analysis",
    subtitle = "sub_target_national",
    help = c("national-global-coverage"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "subnational_target",
    ui = subnational_target_ui,
    server = subnational_target_server,
    title = "title_nav_global_coverage",
    section = "title_nav_subnational_analysis",
    subtitle = "sub_target_subnational",
    help = c("7-subnational-analysis"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "equity_assessment",
    ui = equity_ui,
    server = equity_server,
    title = "title_nav_equity",
    section = "title_nav_national_analysis",
    subtitle = "sub_equity",
    help = c("national-inequality", "interpretation-of-equiplots"),
    denominator = TRUE,
    report = "national_inequality"
  ),
  cd_page_def(
    id = "mortality_institutional",
    ui = mortality_ui,
    server = mortality_server,
    title = "title_mortality_institutional",
    section = "title_mortality",
    subtitle = "sub_mort_institutional",
    help = c("9-mortality"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "mortality_mapping",
    ui = mortality_mapping_ui,
    server = mortality_mapping_server,
    title = "title_mortality_mapping",
    section = "title_mortality",
    subtitle = "sub_mort_mapping",
    help = c("9-mortality"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "mortality_completeness",
    ui = mortality_completeness_ui,
    server = mortality_completeness_server,
    title = "title_mortality_completeness",
    section = "title_mortality",
    subtitle = "sub_mort_completeness",
    help = c("9-mortality"),
    denominator = TRUE,
    report = "mortality"
  ),
  cd_page_def(
    id = "utilization_dqa",
    ui = utilization_dqa_ui,
    server = utilization_dqa_server,
    title = "title_utilization_dqa",
    section = "title_service_utilization",
    subtitle = "sub_util_dqa",
    help = c("10-service-utilisation"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "national_utilization",
    ui = national_service_utilization_ui,
    server = national_service_utilization_server,
    title = "title_national_utilization",
    section = "title_service_utilization",
    subtitle = "sub_util_national",
    help = c("10-service-utilisation"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "subnational_utilization",
    ui = subnational_service_utilization_ui,
    server = subnational_service_utilization_server,
    title = "title_subnational_utilization",
    section = "title_service_utilization",
    subtitle = "sub_util_subnational",
    help = c("10-service-utilisation"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "mch_curative_index",
    ui = mch_curative_index_ui,
    server = mch_curative_index_server,
    title = "title_mch_curative",
    section = "title_service_utilization",
    subtitle = "sub_mch_curative",
    help = c("10-service-utilisation"),
    denominator = TRUE,
    report = "service_utilization"
  ),
  cd_page_def(
    id = "health_system_national",
    ui = health_system_national_ui,
    server = health_system_national_server,
    title = "title_national_health_system",
    section = "opt_health_system_performance",
    subtitle = "sub_hs_national",
    help = c("11-health-system-performance"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "health_system_subnational",
    ui = health_system_subnational_ui,
    server = health_system_subnational_server,
    title = "title_subnational_health_system",
    section = "opt_health_system_performance",
    subtitle = "sub_hs_subnational",
    help = c("11-health-system-performance"),
    denominator = TRUE
  ),
  cd_page_def(
    id = "health_system_comparison",
    ui = health_system_comparison_ui,
    server = health_system_comparison_server,
    title = "title_health_system_comparison",
    section = "opt_health_system_performance",
    subtitle = "sub_hs_comparison",
    help = c("11-health-system-performance"),
    denominator = TRUE,
    report = "health_system"
  ),
  cd_page_def(
    id = "private_sector",
    ui = private_sector_ui,
    server = private_sector_server,
    title = "title_private_sector",
    section = "opt_health_system_performance",
    subtitle = "sub_private_sector",
    help = c("11-health-system-performance"),
    denominator = TRUE,
    report = "private_sector"
  ),
  cd_page_def(
    id = "bayesian_national",
    ui = bayesian_ui,
    server = bayesian_server,
    title = "title_bayesian_analysis",
    section = "title_bayesian_analysis",
    subtitle = "sub_bayesian",
    help = c("5.1-bayesian-coverage"),
    denominator = TRUE,
    server_args = list('national')
  ),
  cd_page_def(
    id = "bayesian_subnational",
    ui = bayesian_ui,
    server = bayesian_server,
    title = "title_bayesian_analysis",
    section = "title_bayesian_analysis",
    subtitle = "sub_bayesian",
    help = c("5.1-bayesian-coverage"),
    denominator = TRUE,
    server_args = list('adminlevel_1')
  ),
  cd_page_def(
    id = "reports",
    ui = reports_ui,
    server = reports_server,
    title = "title_reports",
    section = "lbl_nav_section_output",
    subtitle = "sub_reports",
    help = c("12-dissemination")
  )
)

cd_use_pages(cd_page_registry)
