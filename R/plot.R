# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, main = NULL, sub = NULL, ann = graphics::par("ann"),
                                axes = TRUE, frame.plot = FALSE,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  labels <- rev(names(x))

  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  if (length(col) != nrow(x)) col <- rep(col, length.out = nrow(x))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  resolution <- if (is_CE(x)) x@resolution else x@resolution * -1
  years <- seq(from = x@start, by = resolution, length.out = ncol(x))

  xlim <- range(years)
  xlim <- if (is_CE(x)) xlim else rev(xlim)
  ylim <- c(1, nrow(x) + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  ages <- rev(seq_len(nrow(x)))
  graphics::abline(h = ages, col = "grey")
  for (i in ages) {
    d <- x[i, ]
    d <- (d - min(d)) / max(d - min(d)) * 1.5

    xi <- c(years[1], years, years[length(years)])
    yi <- c(0, d, 0) + i

    graphics::polygon(xi, yi, border = NA, col = col[i])
    graphics::lines(xi, yi, lty = "solid", col = "black")
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::mtext(labels, side = 2, at = ages, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- switch (
      get_calendar(x),
      CE = "Year CE",
      BP = "Year cal. BP",
      b2k = "Year b2k",
      stop("Unknown calendar scale.", call. = FALSE)
    )
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot_14C
#' @aliases plot,CalibratedAges,missing-method
setMethod("plot", c(x = "CalibratedAges", y = "missing"), plot.CalibratedAges)
