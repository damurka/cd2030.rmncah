# What the chart tools (js/src/components/ChartLabels.tsx, ChartView.tsx) do to one chart.
# Everything here works on the ggplot object a plot function returns, so it applies to every chart without the
# chart knowing. A plot that is not a ggplot (drawn with base graphics) is returned unchanged.

# Charts with more categories than this on the horizontal axis are turned so the categories run down the side,
# where their names stay readable, and the chart grows taller to give each one room.
MANY_CATEGORIES <- 12
ROW_HEIGHT_PX <- 26

# The number of categories on each axis of the plot as data (0 for a continuous axis)
chart_axes <- function(p) {
  built <- ggplot2::ggplot_build(p)
  count <- function(scale) {
    if (!is.null(scale) && isTRUE(scale$is_discrete())) length(scale$get_limits()) else 0L
  }
  list(x = count(built$layout$panel_scales_x[[1]]), y = count(built$layout$panel_scales_y[[1]]))
}

# Which way round the chart is drawn, and how tall it should be.
#   flip:    TRUE to swap the axes relative to how the plot function drew it
#   flipped: TRUE when the data's x ends up down the side once drawn (a plot can already be drawn with coord_flip)
#   auto:    TRUE when `flip` was decided here, not chosen by the user
#   rows:    categories down the side once drawn, for the height
chart_layout <- function(p, view = NULL) {
  default <- list(flip = FALSE, flipped = FALSE, auto = TRUE, rows = 0L, height = 400)
  if (!inherits(p, "ggplot")) return(default)

  axes <- tryCatch(chart_axes(p), error = function(e) NULL)
  if (is.null(axes)) return(default)

  already_flipped <- inherits(p$coordinates, "CoordFlip")
  plain <- inherits(p$facet, "FacetNull")
  chosen <- view$flip
  auto <- is.null(chosen)
  flip <- if (auto) (!already_flipped && plain && axes$x > MANY_CATEGORIES) else isTRUE(chosen)

  flipped <- xor(flip, already_flipped)
  rows <- if (flipped) axes$x else axes$y
  height <- if (rows > MANY_CATEGORIES) rows * ROW_HEIGHT_PX + 170 else 400
  list(flip = flip, flipped = flipped, auto = auto, rows = rows, height = height)
}

# The chart's own text, used as placeholders in the label editor. Horizontal / vertical are what is shown, so
# they swap when the chart is turned.
chart_label_defaults <- function(p, flipped = FALSE) {
  if (!inherits(p, "ggplot")) return(list())
  labs <- ggplot2::get_labs(p)
  text <- function(x) if (is.character(x) && length(x) == 1 && !is.na(x)) x else NULL
  legend_key <- intersect(c("fill", "colour", "color", "shape", "linetype", "size", "alpha"), names(labs))
  horizontal <- if (flipped) "y" else "x"
  vertical <- if (flipped) "x" else "y"
  out <- list(
    title = text(labs$title),
    caption = text(labs$caption),
    x = text(labs[[horizontal]]),
    y = text(labs[[vertical]]),
    legend = if (length(legend_key)) text(labs[[legend_key[[1]]]]) else NULL
  )
  Filter(Negate(is.null), out)
}

# Apply the user's label overrides and view options to a plot. `labels` and `view` are what the two components
# report (lists, or NULL when nothing is changed). `layout` is chart_layout(p, view).
apply_chart_options <- function(p, labels = NULL, view = NULL, layout = chart_layout(p, view)) {
  if (!inherits(p, "ggplot")) return(p)

  set <- function(x) if (is.character(x) && length(x) == 1 && nzchar(x)) x else NULL
  replacement <- list()
  replacement$title <- set(labels$title)
  replacement$caption <- set(labels$caption)
  # "horizontal" and "vertical" are what the user sees: swap them onto the data's axes when the chart is turned
  replacement[[if (layout$flipped) "y" else "x"]] <- set(labels$x)
  replacement[[if (layout$flipped) "x" else "y"]] <- set(labels$y)
  if (!is.null(set(labels$legend))) {
    for (key in intersect(c("fill", "colour", "color", "shape", "linetype", "size", "alpha"), names(ggplot2::get_labs(p)))) {
      replacement[[key]] <- labels$legend
    }
  }
  replacement <- Filter(Negate(is.null), replacement)
  if (length(replacement)) p <- p + do.call(ggplot2::labs, replacement)

  # a swap turns a chart drawn one way to the other, so a chart the function already drew flipped is turned back
  if (layout$flip) p <- p + if (inherits(p$coordinates, "CoordFlip")) ggplot2::coord_cartesian() else ggplot2::coord_flip()

  scale <- c(s = 0.85, m = 1, l = 1.25)[[view$size %||% "m"]]
  if (scale != 1) p <- p + ggplot2::theme(text = ggplot2::element_text(size = 11 * scale))

  position <- c(right = "right", bottom = "bottom", hidden = "none")[[view$legend %||% "right"]]
  if (position != "right") p <- p + ggplot2::theme(legend.position = position)

  p
}
