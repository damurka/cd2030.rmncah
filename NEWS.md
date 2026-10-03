# cd2030.rmncah 2.0.8

* Requires cd2030.core 1.3.8 and datasuite.ui 0.4.3: zero-dose, under-vaccinated and measles2 coverage as the
  Countdown 2030 Stata code computes them; every table of the dataset in its notebooks, with what it holds; charts and
  tables already drawn follow a change of language; cards built outside a reactive context no longer fail.
* The same version as cd2030.rmncah, cd2030.vaxx and cd2030.pooled.

# cd2030.rmncah 2.0.7

* The Bayesian analysis's two background R processes start the first time a Bayesian page is opened, not when the app
  starts.
* The Load Data screen is cd2030.core's (`cd_upload_data_ui()` / `cd_upload_data_server()`, cd2030.core 1.3.7), the
  same as the Vaxx app's; this app keeps only its part of the wizard (`rmncah_wizard_options()`).
* Service Utilization's data quality table and the national Health System table are cd2030.core's table card
  (`cd_table_card_ui()` / `cd_table_card_server()`). The national Health System table's picture is now the table on
  screen (the latest year, translated labels; it was the 2024 table in English) and its downloads are named
  `health_system_national`, not `overall_score`.
* A failed reference-data upload (UN, WUENIC or UN mortality estimates) says why, not only that the format is
  unsupported.
* No longer imports flextable or openxlsx directly (cd2030.core does).

# cd2030.rmncah 2.0.6

* The Bayesian analysis installs its model's packages when it is first opened: inside DataSuite with a button
  (DataSuite installs them, showing the progress in its status bar, and offers to restart the app); in plain R the
  page shows the `install.packages()` command. No model is fitted while they are missing.
* Requires cd2030.core 1.3.6.

# cd2030.rmncah 2.0.5

* The report builder is Quire (datasuite.ui 0.4.0, quire 0.2.17): tables can be aligned (numbers and text apart).
* Data Adjustment takes in Remove Years: the years and areas removed, and completeness, outliers and missing values
  set by indicator, region or district (cd2030.core 1.3.5).
* Reports is on the header's button only, not in the sidebar.
* Tables show their own loader.
* The Bayesian analysis runs in the background (an ExtendedTask): the app stays usable while the model fits.
* Requires cd2030.core 1.3.5, datasuite.ui 0.4.0 and quire 0.2.17.

# cd2030.rmncah 2.0.4

* Portuguese: the app reads as Portuguese is written in Mozambique and Angola (European norm) instead of Brazilian Portuguese.

# cd2030.rmncah 2.0.3

* The denominator options read "ANC1 population growth" and "Penta1 population growth" (`anc1derived`,
  `penta1derived`), the labels of cd2030.core's data dictionary. Requires cd2030.core 1.3.1.

# cd2030.rmncah 2.0.2

* "Get help" opens the app guide's pages and sections after the docs reorganisation (`apps/countdown/...`).
* Requires cd2030.core 1.3.0 and datasuite.ui 0.3.0 (the working Ask AI buttons, the AI's confirmations).

# cd2030.rmncah 2.0.1

Documentation only; no change to the app.

* A README: what the app does, installing and running it (`run_app()` arguments and the `CDSUITE_SHINY_*` variables),
  its files, developing and releasing it.

# cd2030.rmncah 2.0.0

* First release as an installable package: `cd2030.rmncah::run_app()` starts the app.
* Built on cd2030.core (>= 1.1.0) and datasuite.ui (>= 0.1.0); the shared Countdown pages, report builder and
  translations now come from those packages instead of copied code.
* R CMD check passes with no errors, warnings or notes.
