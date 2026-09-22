# Choosing several years: a multi-choice chip whose options are the years in the data and whose value is kept in
# step with a setting in the cache. Choosing nothing means "all years" and reaches the server as "".
#
#   UI:      cdChipMulti(ns("years"), "title_global_select_years", i18n = i18n)
#   server:  yearsSelectSync(input, session, "years", years = <reactive of available years>,
#                            selected = <reactive of the years chosen in the cache>)
#            and the module's own observeEvent(input$years, ...) writes the choice to the cache.

# cache -> chip. Pushes the options and the chosen years once the chip has mounted, and again when either changes.
# It depends only on the cache side and the mount, never on input$years: a choice reaches the cache through the
# module's own writer, and an observer that also reacted to the input could run first and push the old value
# back over the user's choice.
yearsSelectSync <- function(input, session, id = "years", years, selected) {
  stopifnot(is.reactive(years), is.reactive(selected))
  mounted <- cdMounted(input, id)

  observeEvent(list(years(), selected(), mounted()), {
    req(mounted(), years())
    chosen <- selected()
    updateCdChip(
      id, session,
      options = cdPlainOptions(years()),
      value = if (length(chosen) && !all(is.na(chosen))) as.character(chosen) else ""
    )
  })
}
