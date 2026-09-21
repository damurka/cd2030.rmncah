$(function () {
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return

  Shiny.addCustomMessageHandler("starting_download", function (msg) {
    var $btn = $("#" + msg.id)
    if (!$btn.length) return

    // keep original html once
    if ($btn.data("orig-html") === undefined) $btn.data("orig-html", $btn.html())

    var html = ''
    html += '<span class="spinner-border spinner-border-sm me-2" role="status" aria-hidden="true"></span>'
    html += '<span class="ps-1">' + (msg.message || "") + '</span>'

    $btn
      .html(html)
      .addClass("disabled")
      .attr("aria-busy", "true")
      .attr("aria-disabled", "true")
      .css({ "pointer-events": "none", opacity: 0.6 })
  })

  Shiny.addCustomMessageHandler("end_download", function (msg) {
    var $btn = $("#" + msg.id)
    if (!$btn.length) return

    var labelHtml = '<i class="bi bi-download me-2"></i>' + (msg.label || "")
    // prefer original if we stored it
    var orig = $btn.data("orig-html")
    $btn
      .html(orig !== undefined ? orig : labelHtml)
      .removeClass("disabled")
      .removeAttr("aria-busy aria-disabled")
      .css({ "pointer-events": "auto", opacity: 1 })
  })
})

Shiny.addCustomMessageHandler('reinit-tooltips', function (ms) {
  $('body').tooltip({ selector: '[data-toggle=\"tooltip\"]', html: false, container: 'body' });
})
