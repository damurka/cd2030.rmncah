# The Bayesian page fits its models in other R processes (an ExtendedTask with future, R/page-3_bayesian.R), so the
# app is not frozen meanwhile. The workers are started the first time a Bayesian page is opened with data loaded (its
# `active()`), before its first fit -- not when the app starts, as most sessions never open the page.

# The Bayesian page as the page registry starts it (R/pages.R): bayesian_server() with an `active` that starts the
# workers the first time it is TRUE.
bayesian_page_server <- function(id, cache, i18n, admin_level, active = reactive(TRUE)) {
  stopifnot(is.reactive(active))
  bayesian_server(id, cache, i18n, admin_level, active = reactive({
    on <- active()
    if (isTRUE(on)) rmncah_start_workers()
    on
  }))
}

# Two workers, started once for the R session, unless it already has a plan of its own.
rmncah_start_workers <- function() {
  if (inherits(future::plan(), "sequential")) future::plan(future::multisession, workers = 2L)
  invisible(future::plan())
}
