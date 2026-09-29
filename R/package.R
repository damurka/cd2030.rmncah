# quire (the report builder, which datasuite.ui shows) is imported here for its version: the app's DESCRIPTION says
# which quire its users get (datasuite.ui asks only for what its own code needs).
#' @keywords internal
#' @import cd2030.core
#' @import datasuite.ui
#' @import shiny
#' @importFrom quire quire_ui
#' @importFrom dplyr %>% arrange distinct pull select
#' @importFrom flextable htmltools_value save_as_image
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom stringr str_remove
"_PACKAGE"
