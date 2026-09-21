renderCustomPlot <- function(expr) {
  # Helper function to generate an error plot
  generate_error_plot <- function(message, color = 'red') {
    graphics::plot.new()
    graphics::text(
      x = 0.5, y = 0.5,
      labels = message,
      cex = 1.2, col = color
    )
  }

  # Convert expression to a function
  func <- tryCatch({
    as_function(enquo(expr))
  }, error = function(e) {
    function() generate_error_plot(paste('Error parsing expression:', clean_error_message(e)))
  })

  renderPlot({
    # Evaluate the data from func
    check_data <- tryCatch(
      eval_tidy(enquo(expr)),
      error = function(e) e
    )

    # Check if data is empty or invalid
    if (inherits(check_data, "error") ||
        (is.data.frame(check_data) && nrow(check_data) == 0) ||
        (is.vector(check_data) && length(check_data) == 0) ||
        (is.matrix(check_data) && nrow(check_data) == 0)) { #Added matrix check

      message <- clean_error_message(check_data)

      if (nchar(message) == 0 || message == '') {
        generate_error_plot('No data available', 'gray')
      } else {
        print(check_data)
        generate_error_plot(message, 'red')
      }
      return() # Return early, no progress or further processing
    }
    tryCatch({
      if (inherits(check_data, "ggplot")) {
        print(check_data) # ggplot object
      } else if (inherits(check_data, "plotly")) {
        plotly::plotlyOutput(check_data)
      } else {
        func() # Base R plot
      }
    },
    error = function(e) {
      print(e)
      # if (inherits(e, 'shiny.silent.error')) return()
      generate_error_plot(paste('Error:', clean_error_message(e)))
    })
  })
}

plotCustomOutput <- function(id) {
  withSpinner(plotOutput(id))
}
