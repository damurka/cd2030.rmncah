# cd2030.rmncah

The Countdown to 2030 **RMNCAH** app: a Shiny app for the end-to-end analysis of a country's routine health facility
data for reproductive, maternal, newborn, child and adolescent health. Users load and check the data, choose
denominators, analyse coverage, equity, mortality, service use and the health system, and build Word, PowerPoint and
PDF reports. In English, French and Portuguese.

It runs inside DataSuite (through the [countdown-analytics](https://github.com/damurka/countdown-analytics) extension)
or on its own in R. How this package fits with the others, and how a release reaches users:
[countdown-analytics/docs/ARCHITECTURE.md](https://github.com/damurka/countdown-analytics/blob/main/docs/ARCHITECTURE.md).

## Install and run

```r
install.packages("cd2030.rmncah", repos = c("https://damurka.r-universe.dev", "https://alkemalab.r-universe.dev",
                                            "https://cloud.r-project.org"))
cd2030.rmncah::run_app()                                   # prints (runs) the app
cd2030.rmncah::run_app(selected_file = "data.xlsx", language = "fr")
```

`run_app()` returns a Shiny app object; printing it or `shiny::runApp()` runs it. Its arguments default to the
environment variables DataSuite sets when it launches the app:

| Argument | Environment variable | |
| --- | --- | --- |
| `selected_file` | `CDSUITE_SHINY_SELECTED_FILE` | the file the user picked: facility data (`.xls`, `.xlsx`, `.dta`) or a saved dataset (`.rds`); `NA` for none |
| `language` | `CDSUITE_SHINY_LOCALE` | `"en"`, `"fr"` or `"pt"` |
| `app_name` | `CDSUITE_SHINY_NAME` | the name in the header |
| `app_version` | `CDSUITE_SHINY_VERSION` | the version in the header; defaults to this package's version |
| `...` | | passed to `shiny::shinyApp()`'s `options` (e.g. `port`) |

In DataSuite nothing needs installing by hand: DataSuite installs this package when the extension is installed and
updates it when the extension is updated.

## What is in it

| Section | Pages |
| --- | --- |
| Start | Introduction (`inst/intro`), Load Data (the wizard, from cd2030.core, with this app's options: `rmncah_wizard_options()`) |
| Data quality | reporting rate, completeness, internal consistency, outliers, overall score, remove years, adjustment, adjustment changes |
| Denominators | assessment, selection |
| Analysis | national and subnational coverage, continuum of care, inequality, targets, equity; mortality (institutional, mapping, completeness); service utilization (data quality, national, subnational, MCH curative index); health system (national, subnational, comparison, private sector); Bayesian coverage model (national, subnational) |
| Reports | the report builder |

Most pages are shared with the Vaxx app and live in cd2030.core (`R/ui-*.R`). This package holds only what is RMNCAH's:

| File | What |
| --- | --- |
| `R/run_app.R` | `run_app()`: the indicator group (`rmncah`), the app's config (`options(cd2030.config = ...)`: which indicators each shared page shows, adjustment k-factors, reporting-rate indicators...), translations, the nav tree, then `cd2030.core::cd_app()` |
| `R/pages.R` | `rmncah_pages()`: every page (id, UI, server, title, section, help topic, report) |
| `R/page-*.R` | the RMNCAH-only pages: continuum of care, mortality, service utilization, MCH curative index, health system, private sector, and this app's Load Data options |
| `inst/translation/translation.json` | the app's own texts (merged over datasuite.ui's and cd2030.core's with `cd_translations()`) |
| `inst/intro/0_intro_<lang>.md` | the Introduction page |
| `app.R` | runs the app from this folder (see below); not part of the package |
| `data-reference.json`, `docs-index.json` | what DataSuite's AI tools read; copies live in the extension, not part of the package |

The Bayesian pages need `bayescoveragemodel` and `bayescoveragedeploy` (suggested by cd2030.core, from
https://alkemalab.r-universe.dev).

## Develop

```r
shiny::runApp()          # from this folder: app.R loads the source with pkgload::load_all()
devtools::check()        # must stay at 0 errors, 0 warnings, 0 notes
```

`app.R` uses `pkgload::load_all()` when it finds `DESCRIPTION` next to it, otherwise the installed package. To try a
change to cd2030.core or datasuite.ui at the same time, install that package locally first (for cd2030.core:
`devtools::install(quick = TRUE, upgrade = FALSE, dependencies = FALSE)`, so its `Remotes:` don't reinstall
datasuite.ui from GitHub).

Keep `R/` ASCII (`\uXXXX` escapes in strings): R CMD check warns otherwise. Column names used unquoted in dplyr code go
in `R/globals.R`.

A page RMNCAH and Vaxx both need belongs in cd2030.core, not here.

## Release

1. Bump `Version:` in DESCRIPTION, add a NEWS.md entry, `devtools::check()` clean.
2. Commit, tag `vX.Y.Z`, push `main` and the tag. r-universe builds it within about an hour (instantly with its GitHub
   app installed).
3. DataSuite users get it at the next update of the countdown-analytics extension. If they must have this version,
   raise `package.version` for RMNCAH in the extension's package.json and release the extension.
