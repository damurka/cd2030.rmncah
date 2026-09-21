$(function(){
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return

  Shiny.addCustomMessageHandler('messagebox', function(msg){
    var $root = $('#' + msg.rootId)
    if (!$root.length) return

    var $stack = $root.find('.messages-stack').first()
    if (!$stack.length) return

    if (msg.action === 'clear') {
      $stack.empty()
      return
    }

    var $el = $(msg.usePre ? '<pre>' : '<div>')
      .addClass('msg ' + (msg.status || 'info'))
      .text(msg.text || '')
      .attr('role', msg.status === 'error' ? 'alert' : 'status')

    $stack.append($el)
    $stack.scrollTop($stack[0].scrollHeight)
  })
})