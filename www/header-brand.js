$(function(){
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return

  Shiny.addCustomMessageHandler('setHeaderBrand', function(msg){
    var $bar = $('header.main-header .navbar')
    if (!$bar.length) return

    var $toggle = $bar.find('.sidebar-toggle').first()
    if (!$toggle.length) return

    var $wrap = $bar.find('.navbar-header')
    if (!$wrap.length) $wrap = $('<div class="navbar-header"></div>').insertAfter($toggle)    

    var $brand = $wrap.find('#hdr-brand')
    if (!$brand.length) $brand = $('<h4 id="hdr-brand" class="navbar-brand"></h4>').appendTo($wrap)

    $brand.html(msg.html || '')
  })
})
