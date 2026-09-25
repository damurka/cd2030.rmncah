#' Run the RMNCAH app
#'
#' The Countdown RMNCAH app: the Load Data wizard, the data quality, denominator and analysis pages, and the report
#' builder. DataSuite starts it with its settings in environment variables (`CDSUITE_SHINY_NAME`, `_VERSION`, `_LOCALE`,
#' `_SELECTED_FILE`); they can be given as arguments instead.
#'
#' @param selected_file A dataset to open (DataSuite's `CDSUITE_SHINY_SELECTED_FILE`), or `NA`.
#' @param language The starting language: `"en"`, `"fr"` or `"pt"`.
#' @param app_name,app_version The name and version shown in the header.
#' @param ... Passed to [shiny::shinyApp()]'s `options` (e.g. `port`, `launch.browser`).
#' @return A Shiny app object: printing it (or [shiny::runApp()]) runs it.
#' @export
run_app <- function(selected_file = Sys.getenv("CDSUITE_SHINY_SELECTED_FILE", unset = NA),
                    language = Sys.getenv("CDSUITE_SHINY_LOCALE", unset = "en"),
                    app_name = Sys.getenv("CDSUITE_SHINY_NAME", unset = "RMNCAH"),
                    app_version = Sys.getenv("CDSUITE_SHINY_VERSION", unset = as.character(utils::packageVersion("cd2030.rmncah"))),
                    ...) {
  # uploads include saved datasets, not just the facility data
  options(shiny.maxRequestSize = 2 * 1024 * 1024^2, future.globals.maxSize = 3 * 1024 * 1024^2, shiny.fullstacktrace = TRUE)

  # cd2030.core keeps ONE indicator group for the whole R session (set_selected_group()); this app's own copy
  # (cd2030.app_group) is one loading a dataset cannot change
  options(cd2030.selected_group = "rmncah", cd2030.app_group = "rmncah")
  set_selected_group("rmncah")

  # What is particular to this app for the shared page modules (R/ui-core-config.R in cd2030.core lists the keys).
  options(cd2030.config = list(
    nat_cov_indicators = c("anc4", "instlivebirths", "low_bweight", "penta3", "measles1", "fpet"),
    target_indicators = c("anc4", "instlivebirths", "vaccine"),
    equity_indicators = c("anc4", "instlivebirths", "low_bweight", "penta3", "measles1"),
    cov_trend_indicators = c("instlivebirths", "penta3"),
    sub_derived_indicators = c("instlivebirths", "penta3"),
    survey_comp_indicators = c("instlivebirths", "penta3"),
    adjustment_indicators = c("ideliv", "instlivebirths", "penta1", "anc1", "opd_under5"),
    k_factors = list(
      anc = list(id = "k_anc", label = "title_adjust_anc_factor"),
      idelv = list(id = "k_delivery", label = "title_adjust_delivery_factor"),
      vacc = list(id = "k_vaccines", label = "title_adjust_vaccines_factor"),
      opd = list(id = "k_opd", label = "title_adjust_opd_factor")
    ),
    reporting_rate_indicators = c("opt_anc" = "anc_rr", "opt_idelv" = "idelv_rr", "opt_vacc" = "vacc_rr", "opt_opd" = "opd_rr"),
    reporting_rate_facet_ncol = 2,
    consistency_pairs = list(c("anc1", "penta1"), c("penta1", "penta3")),
    has_maternal = TRUE
  ))
  rmncah_wizard_options()

  # the Introduction page's help, in each language
  options(cd2030.help_dir = system.file("intro", package = "cd2030.rmncah"))

  i18n <- shiny.i18n::init_i18n(translation_json_path = cd_translations(system.file("translation", "translation.json", package = "cd2030.rmncah")))
  i18n$set_translation_language(language)
  cd_use_i18n(i18n)

  # every analysis page, in one list (R/pages.R)
  pages <- rmncah_pages()
  cd_use_pages(pages)

  # the sidebar/header nav tree: the standard sections (cd2030.core) plus this app's own groups
  nav <- list(
    cd_nav_start(),
    cd_nav_quality(),
    cd_nav_denominators(),
    cd_nav_section("lbl_nav_section_analysis",
      cd_nav_national(extra = list(cd_nav_item("title_continuum", tabName = "continuum_care", icon = "heart-pulse"))),
      cd_nav_subnational(),
      # "balance-scale" -- a weighing scale -- had no connection to mortality at all, and was also Service
      # Utilization's icon below, so the two were indistinguishable in the sidebar besides. Was "heart-crack"
      # after that; Font Awesome Free has no outline/regular weight for it (Pro-only), so it always rendered
      # solid regardless -- plain "heart" keeps the same body part/theme and has one. "heart-pulse" (a heartbeat
      # line) was ruled out here as a substitute: it's already Continuum of Care's icon a few rows up.
      cd_nav_item("title_mortality", icon = "heart", requires_adjustment = TRUE, children = list(
        cd_nav_item("title_mortality_institutional", tabName = "mortality_institutional", icon = "hospital"),
        cd_nav_item("title_mortality_mapping", tabName = "mortality_mapping", icon = "map-marked"),
        # "user-slash" again for *completeness* -- a checked clipboard is what a completeness page's icon
        # should say, matching Data Completeness's family above.
        cd_nav_item("title_mortality_completeness", tabName = "mortality_completeness", icon = "clipboard-check")
      )),
      # A stethoscope for service utilization, not the same weighing scale Mortality used a moment ago.
      cd_nav_item("title_service_utilization", icon = "stethoscope", requires_adjustment = TRUE, children = list(
        cd_nav_item("title_utilization_dqa", tabName = "utilization_dqa", icon = "clipboard-check"),
        cd_nav_item("title_national_utilization", tabName = "national_utilization", icon = "chart-area"),
        cd_nav_item("title_subnational_utilization", tabName = "subnational_utilization", icon = "map-marked"),
        cd_nav_item("title_mch_curative", tabName = "mch_curative_index", icon = "notes-medical")
      )),
      # "globe-africa" here duplicated Sub-National Analysis's old icon; sitemap reads as "the health system as a
      # structure", which is what this group's pages are actually about.
      cd_nav_item("opt_health_system_performance", icon = "sitemap", requires_adjustment = TRUE, children = list(
        cd_nav_item("title_national_health_system", tabName = "health_system_national", icon = "building-columns"),
        cd_nav_item("title_subnational_health_system", tabName = "health_system_subnational", icon = "building"),
        # "user-slash" for a *comparison* page, and again for Private Sector right below it -- two unrelated
        # pages sharing one icon that meant neither of them.
        cd_nav_item("title_health_system_comparison", tabName = "health_system_comparison", icon = "arrows-left-right"),
        cd_nav_item("title_private_sector", tabName = "private_sector", icon = "briefcase")
      )),
      cd_nav_item("title_bayesian_analysis", icon = "chart-area", requires_adjustment = TRUE, children = list(
        # Not "flag"/"map" here -- those are National Analysis's and Sub-National Analysis's own top-level rail
        # icons above; reusing them on a *different* top-level item made the icon rail ambiguous at a glance.
        cd_nav_item("title_nav_national_analysis", tabName = "bayesian_national", icon = "earth-americas"),
        cd_nav_item("title_nav_subnational_analysis", tabName = "bayesian_subnational", icon = "map-location-dot")
      ))
    ),
    # Reports built from blocks of this app's charts and tables (datasuite.ui, R/kit-reports.R)
    cd_nav_section("lbl_nav_section_output",
      cd_nav_item("title_reports", tabName = "reports", icon = "file-lines", requires_adjustment = TRUE)
    )
  )

  app <- cd_app(
    app_name = app_name, app_version = app_version, theme = "rmncah",
    nav_sections = nav, registry = pages,
    i18n = i18n, language = language, selected_file = selected_file,
    upload_ui = upload_data_ui, upload_server = upload_data_server
  )
  if (length(list(...))) app$options <- utils::modifyList(app$options %||% list(), list(...))
  app
}
