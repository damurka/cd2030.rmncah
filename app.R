# Runs the RMNCAH app from this folder: the package's source when developing (pkgload), else the installed cd2030.rmncah.
# The app itself is the package cd2030.rmncah (R/run_app.R); installed, it runs with cd2030.rmncah::run_app().
if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
  library(cd2030.rmncah)
}
run_app()
