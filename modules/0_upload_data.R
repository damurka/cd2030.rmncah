# The Load Data wizard itself is shared (cd2030.core, R/ui-wizard-*.R); this is the part of it that
# is rmncah's own -- see wizard-config.R for the contract. Survey estimates here are the maternal + immunization set
# (anc1, anc4, instlivebirths, csection, low_bweight, bcg, penta1, penta3, measles1); every group below is required
# before the wizard's National Rates step lets you continue.
options(cd2030.wizard = list(
  national_rates_groups = list(
    cd_wizard_field_group("title_upload_group_maternal", "sub_upload_group_maternal",
      cd_wizard_survey_field("anc1_prop", "anc1", "title_upload_anc1_survey"),
      cd_wizard_survey_field("anc4_prop", "anc4", "title_upload_anc4_survey"),
      cd_wizard_survey_field("ideliv_prop", "instlivebirths", "title_upload_ideliv_survey"),
      cd_wizard_survey_field("csection_prop", "csection", "title_upload_csection_survey"),
      cd_wizard_survey_field("lbw_prop", "low_bweight", "title_upload_lbw_survey"),
      cd_wizard_national_rate_fields()
    ),
    cd_wizard_field_group("title_upload_group_immunization", "sub_upload_group_immunization",
      cd_wizard_survey_field("bcg_prop", "bcg", "title_upload_bcg_survey"),
      cd_wizard_survey_field("penta1_prop", "penta1", "title_upload_penta1_survey"),
      cd_wizard_survey_field("penta3_prop", "penta3", "title_upload_penta3_survey"),
      cd_wizard_survey_field("measles1_prop", "measles1", "title_upload_measles1_survey")
    )
  ),
  reference_uploads = c("un_estimates", "wuenic_estimates", "un_mortality_estimates")
))

upload_data_ui <- function(id, i18n, is_electron = FALSE) {
  ns <- NS(id)

  tagList(
    # No cd_page_body() here: this page has no filter bar (there's no dataset loaded yet to filter), just
    # the page header + the wizard shell.
    cd_page_header(
      id = ns("load_data"),
      title = i18n$t("title_nav_load_data"),
      i18n = i18n,
      eyebrow = "lbl_nav_section_start",
      subtitle = "sub_load_data_main",
      include_help = TRUE
    ),
    cd_page_content(
      wizard_steps_ui(ns("wizard"))
    )
  )
}

upload_data_server <- function(id, i18n, cdsuite_file, active) {
  stopifnot(is.reactive(active))
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      cache <- reactiveVal()
      # Mirrors upload_box.R's own reactiveVal(TRUE) default (see its own comment) -- stays TRUE (walkthrough)
      # until upload_dt$requires_walkthrough() reports otherwise, right when cache() itself first gets a value.
      requires_walkthrough <- reactiveVal(TRUE)

      cd_page_header_server("load_data", cache = cache, path = "loading-data", i18n = i18n)

      is_electron <- !is.na(cdsuite_file)
      upload_dt <- upload_box_server("upload_box", i18n, cdsuite_file, is_electron = is_electron)

      # Neither req(upload_dt$cache()) NOR observeEvent()'s own default ignoreNULL=TRUE can gate this: both
      # silently skip the WHOLE observer whenever upload_dt$cache() is NULL, which is exactly the value a
      # cleared dataset (upload_box.R's own input$hfd_file_reset observer) needs to propagate. Confirmed live,
      # twice: with req() alone removed but ignoreNULL still defaulting TRUE, upload_box.R's own initial_cache
      # correctly went back to NULL (traced with a temporary cat()) but this observer never re-ran at all --
      # this page's own cache() reactiveVal, which every other step server actually reads, stayed stuck on the
      # old value forever. ignoreNULL = FALSE is required for a "the whole thing got cleared" signal to ever
      # reach here.
      observeEvent(upload_dt$cache(), {
        cache(upload_dt$cache())
        requires_walkthrough(upload_dt$requires_walkthrough())
      }, ignoreNULL = FALSE)

      # Every step server is instantiated here at startup, whether or not its own panel is the one currently
      # shown -- same "servers always run" convention app.R itself documents for full pages (each one's own
      # req(cache())/req(data()) guards keep it inert until there's something to react to).
      national_rates_server("national_rates", cache, i18n)
      reference_estimates_server("reference_estimates", cache, i18n)
      survey_upload_server("survey_upload", cache, i18n)
      shapefile_step_server("shapefile_step", cache, i18n)
      map_survey_server("map_survey", cache, i18n)
      map_shapefile_server("map_shapefile", cache, i18n)
      data_quality_server("data_quality", cache, i18n)

      # One panel per wizard_step_defs entry (step_status.R), same order -- built with THIS module's own ns(),
      # not wizard_steps_server()'s nested one: the markup just gets embedded inside its cd_tab_panes() wherever
      # that ends up in the DOM, the same way file_upload.R's old UI was already built with its caller's ns()
      # and simply placed inside cd_page_content() -- static UI content doesn't need to share a server module's
      # own namespace, only a *nested moduleServer() call* would.
      panels <- list(
        list(key = "upload", ui = cd_card(
          title = i18n$t("title_upload_dataset_card"),
          subtitle = i18n$t("sub_upload_dataset_card"),
          status = "success",
          solidHeader = TRUE,
          width = 12,
          upload_box_ui(ns("upload_box"), i18n, is_electron),
          reference_estimates_ui(ns("reference_estimates"), i18n)
        )),
        list(key = "quality", ui = data_quality_ui(ns("data_quality"), i18n)),
        list(key = "national_rates", ui = national_rates_ui(ns("national_rates"), i18n)),
        list(key = "survey_files", ui = survey_upload_ui(ns("survey_upload"), i18n)),
        list(key = "shapefile", ui = shapefile_step_ui(ns("shapefile_step"), i18n)),
        list(key = "survey_mapping", ui = map_survey_ui(ns("map_survey"), i18n)),
        list(key = "map_mapping", ui = map_shapefile_ui(ns("map_shapefile"), i18n))
      )

      wizard_steps_server(
        "wizard", i18n = i18n, cache = cache, requires_walkthrough = requires_walkthrough, panels = panels,
        source_path = upload_dt$source_path, active = active
      )

      # requires_walkthrough exposed alongside cache -- app.R's own language-sync observer needs it to tell a
      # fresh Excel/Stata upload (should adopt whatever language is already showing) apart from a resumed .rds
      # (should impose its own saved language instead). See app.R's own comment on that observer.
      list(cache = cache, requires_walkthrough = requires_walkthrough)
    }
  )
}
