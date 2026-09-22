# R side of the Countdown React components, rendered through shiny.react.
#
# Source: js/ (TypeScript, Babel + webpack, components in js/src/components). Build with `npm run build` from
# js/; the built bundle, www/cd-react/cd-react.js, is committed so running the app never needs Node.
# shiny.react's reactElement() attaches React itself, so the bundle only adds our components.
#
# Text: the app translates by scanning the page for elements marked class = "i18n" and data-key and swapping
# their text in the browser. React owns the text it renders, so a component receives every piece of its text
# in all languages ({en, fr, pt}, see cdText()) and picks one itself. When the language changes the server
# sends one "cd-lang" message (see show_language() in app.R) and every component re-renders. Nothing has to be
# pushed to individual components.

# The app's translator, kept here because app.R's own objects live in the app's environment, which the
# functions sourced from ui/ cannot see. app.R calls cd_use_i18n() once, right after creating it.
.cd_state <- new.env(parent = emptyenv())
cd_use_i18n <- function(i18n) assign("i18n", i18n, envir = .cd_state)
cd_i18n <- function() get("i18n", envir = .cd_state)

# The compiled bundle, as an htmltools dependency. Its head script gives the starting language.
cd_react_dependency <- function() {
  start_lang <- cd_i18n()$get_translation_language()
  htmltools::htmlDependency(
    name = "countdownReact",
    version = "0.2.0",
    src = c(file = normalizePath("www/cd-react", mustWork = TRUE)),
    script = "cd-react.js",
    head = sprintf("<script>window.cdLang = window.cdLang || '%s';</script>", start_lang)
  )
}

# A shiny.react element backed by js/src/components/<name>.tsx (registered in js/src/index.ts).
# Every widget wrapper below goes through this, so call sites never touch shiny.react directly.
cd_react_element <- function(name, props) {
  shiny.react::reactElement(module = "@/countdown", name = name, props = props, deps = cd_react_dependency())
}

# A translation key given either as the key or as the markup i18n$t() returns for it
cd_key <- function(x) {
  if (inherits(x, "shiny.tag")) htmltools::tagGetAttribute(x, "data-key") else x
}

# Plain-text translation. i18n$translate() returns markup while the page is being built, so look the
# text up directly. `lang` defaults to the translator's language.
i18n_plain <- function(i18n, key, lang = NULL) {
  lang <- lang %||% i18n$get_translation_language()
  tr <- i18n$get_translations()
  if (!key %in% rownames(tr) || !lang %in% names(tr)) return(key)
  val <- tr[key, lang]
  if (is.na(val) || !nzchar(val)) key else val
}

# One translation key as text in every language: list(en = , fr = , pt = )
cdText <- function(i18n, key) {
  langs <- setdiff(i18n$get_languages(), "key")
  stats::setNames(lapply(langs, function(l) i18n_plain(i18n, key, l)), langs)
}

# choices: a named vector, as for i18nSelectizeInput() -- names are translation keys, values are option values
cdOptions <- function(choices, i18n = cd_i18n()) {
  purrr::pmap(
    list(key = names(choices), value = unname(choices)),
    function(key, value) list(key = as.character(value), text = cdText(i18n, key))
  )
}

# Options that are data, not translations: regions, years. `groups` (optional) gives each a heading.
cdPlainOptions <- function(values, groups = NULL) {
  values <- as.character(values)
  purrr::map(seq_along(values), function(i) {
    o <- list(key = values[[i]], text = values[[i]])
    if (!is.null(groups)) o$group <- as.character(groups[[i]])
    o
  })
}

# Text every chip shares
cdChipTexts <- function(i18n) {
  list(
    resetLabel = cdText(i18n, "lbl_chip_reset"),
    searchLabel = cdText(i18n, "lbl_chip_search"),
    emptyLabel = cdText(i18n, "lbl_chip_no_match")
  )
}

# A compact single-choice filter chip: shows "label  value" and opens a small popover with the options.
# `label` and `hint` are translation keys. `options` overrides `choices` when the options are built in code
# (grouped or data-driven lists). Use "" for `selected` when the value comes from the cache. `default` (optional)
# makes the chip show when it has been changed, and offer a Reset.
cdChipSelect <- function(inputId, label, choices = NULL, i18n = cd_i18n(), selected = NULL, hint = NULL,
                         default = NULL, options = NULL, key = NULL) {
  options <- options %||% if (is.null(choices)) list() else cdOptions(choices, i18n)
  selected <- selected %||% (if (length(options)) options[[1]]$key else "")
  cd_react_element("ChipSelect", do.call(shiny.react::asProps, c(
    list(inputId = inputId, value = selected, options = options, label = cdText(i18n, cd_key(label))),
    if (!is.null(hint)) list(hint = cdText(i18n, cd_key(hint))),
    if (!is.null(default)) list(defaultValue = default),
    # a changed `key` makes React remount the chip, which resets it to `selected`
    if (!is.null(key)) list(key = key),
    cdChipTexts(i18n)
  )))
}

# A multi-choice chip, e.g. several years. Choosing nothing means "all" and reaches the server as "", which is
# what the "All years" entry it replaces sent.
cdChipMulti <- function(inputId, label, choices = NULL, i18n = cd_i18n(), selected = NULL, hint = NULL,
                        options = NULL, all_label = "lbl_all_years") {
  options <- options %||% if (is.null(choices)) list() else cdOptions(choices, i18n)
  cd_react_element("ChipMulti", do.call(shiny.react::asProps, c(
    list(inputId = inputId, value = if (length(selected)) as.character(selected) else "", options = options,
         label = cdText(i18n, cd_key(label)), allLabel = cdText(i18n, all_label)),
    if (!is.null(hint)) list(hint = cdText(i18n, cd_key(hint))),
    cdChipTexts(i18n)
  )))
}

# A chip holding one number, e.g. a threshold
cdChipNumber <- function(inputId, label, i18n = cd_i18n(), value = NULL, min = NULL, max = NULL, step = NULL,
                         unit = "", picks = NULL, default = NULL, hint = NULL) {
  cd_react_element("ChipNumber", do.call(shiny.react::asProps, c(
    list(inputId = inputId, value = value, label = cdText(i18n, cd_key(label)), unit = unit),
    if (!is.null(hint)) list(hint = cdText(i18n, cd_key(hint))),
    if (!is.null(min)) list(min = min),
    if (!is.null(max)) list(max = max),
    if (!is.null(step)) list(step = step),
    if (!is.null(picks)) list(picks = as.list(picks)),
    if (!is.null(default)) list(defaultValue = default),
    cdChipTexts(i18n)
  )))
}

# Push a new value, new options or new text to a chip that is already on the page. The chip must have mounted:
# a message to one that has not is lost, so wait for cdMounted().
updateCdChip <- function(inputId, session = shiny::getDefaultReactiveDomain(), ...) {
  # list2() so callers can splice a list of props in with !!!
  do.call(shiny.react::updateReactInput, c(list(session = session, inputId = inputId), rlang::list2(...)))
}

# TRUE once the React component `id` has mounted in the browser. Call it from the module server that owns it.
cdMounted <- function(input, id) {
  mounted <- reactiveVal(FALSE)
  observeEvent(input[[paste0(id, "__mounted")]], mounted(TRUE), once = TRUE)
  mounted
}

# Tell every React component the language changed
cdSetLanguage <- function(session, lang) {
  session$sendCustomMessage("cd-lang", lang)
}

# The one-line, sticky bar that holds a page's filter chips
cdFilterBar <- function(..., i18n = cd_i18n(), lead = "lbl_filter_scope_page") {
  div(
    class = "cd-filterbar",
    role = "group",
    tags$span(class = "cd-filterbar__lead i18n", `data-key` = lead, i18n_plain(i18n, lead)),
    ...
  )
}

# ---- chart tools -------------------------------------------------------------------------------------------
# Label editor and "this chart only" view for one chart. Their values are Shiny inputs (input$<id>); see
# apply_chart_options() in ui/react/chart-options.R for what the server does with them.

cdChartLabels <- function(inputId, i18n = cd_i18n()) {
  fields <- c(title = "lbl_chart_f_title", caption = "lbl_chart_f_caption", x = "lbl_chart_f_x", y = "lbl_chart_f_y", legend = "lbl_chart_f_legend")
  cd_react_element("ChartLabels", shiny.react::asProps(
    inputId = inputId,
    editable = TRUE,
    defaults = list(),
    texts = list(
      tool = cdText(i18n, "lbl_chart_tool_labels"),
      title = cdText(i18n, "lbl_chart_labels_title"),
      hint = cdText(i18n, "lbl_chart_labels_hint"),
      reset = cdText(i18n, "lbl_chart_reset"),
      resetAll = cdText(i18n, "lbl_chart_reset_all"),
      note = cdText(i18n, "lbl_chart_labels_note"),
      unavailable = cdText(i18n, "lbl_chart_labels_unavailable"),
      fields = lapply(fields, function(k) cdText(i18n, k))
    )
  ))
}

cdChartView <- function(inputId, i18n = cd_i18n()) {
  keys <- c(tool = "lbl_chart_tool_view", title = "lbl_chart_view_title", hint = "lbl_chart_view_hint", reset = "lbl_chart_reset",
            orientation = "lbl_chart_orientation", auto = "lbl_chart_auto", swap = "lbl_chart_swap", keep = "lbl_chart_keep",
            size = "lbl_chart_size", legend = "lbl_chart_legend", right = "lbl_chart_right", bottom = "lbl_chart_bottom",
            hidden = "lbl_chart_hidden")
  cd_react_element("ChartView", shiny.react::asProps(
    inputId = inputId,
    texts = lapply(keys, function(k) cdText(i18n, k))
  ))
}
