use_tooltips <- function() {
  tags$script(HTML("
    $(function(){
      $('[data-toggle=\"popover\"]').popover({
        container: 'body',
        html: true,
        trigger: 'hover',
        content: function () {
          return $(this).find('.popover-i18n').html() || $(this).find('.popover-i18n').text() || '';
        }
      });
    });
  "))
}

tooltip_icon <- function(text, placement = "right", icon = "info-circle") {
  tags$i(
    class = paste0("fa fa-", icon),
    `data-toggle` = "popover",
    `data-placement` = placement,
    tabindex = 0,
    role = "button",
    style = "cursor:pointer;margin-left:6px;",
    tags$span(
      class = "i18n popover-i18n",
      `data-key` = text,
      style = "display:none;",
      text
    )
  )
}

tooltip_label <- function(label, text, placement = "right", icon = "info-circle") {
  tagList(tags$label(class = "control-label i18n", `data-key` = label, label), tooltip_icon(text, placement, icon))
}
