# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, interval = TRUE, level = 0.95,
                                decreasing = TRUE,
                                main = NULL, sub = NULL, ann = graphics::par("ann"),
                                axes = TRUE, frame.plot = FALSE,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  lty <- list(...)$col %||% graphics::par("lty")
  lwd <- list(...)$col %||% graphics::par("lwd")
  tcl <- list(...)$col %||% graphics::par("tcl")
  if (length(col) != nrow(x)) col <- rep(col, length.out = nrow(x))
  fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- get_years(x)

  xlim <- range(years)
  xlim <- if (is_CE(x)) xlim else rev(xlim)
  ylim <- c(1, nrow(x) + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder
  mid <- median(x)
  if (!is_BP(x) && !is_b2k(x)) decreasing <- !decreasing
  x <- x[order(mid, decreasing = !decreasing), , drop = FALSE]

  ## Plot
  ages <- rev(seq_len(nrow(x)))
  graphics::abline(h = ages, col = "grey")
  for (i in ages) {
    d <- x[i, , drop = TRUE]

    d <- (d - min(d)) / max(d - min(d)) * 1.5
    k <- which(d > 0) # Keep only density > 0
    lb <- if (min(k) > 1) min(k) - 1 else min(k)
    ub <- if (max(k) < length(years)) max(k) + 1 else max(k)
    xi <- c(years[lb], years[k], years[ub])
    yi <- c(0, d[k], 0) + i

    graphics::polygon(xi, yi, border = NA, col = fill[i])
    graphics::lines(xi, yi, lty = "solid", col = "black")
  }
  if (interval) {
    hpd <- hpdi(x, level = level)
    for (i in ages) {
      h <- hpd[[i]]
      graphics::segments(
        x0 = h[, "lower"], x1 = h[, "upper"],
        y0 = i, y1 = i,
        col = "black", lty = lty, lwd = lwd
      )
      graphics::segments(
        x0 = c(h[, "lower"], h[, "upper"]), x1 = c(h[, "lower"], h[, "upper"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = "black", lty = lty, lwd = lwd
      )
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::mtext(names(x)[ages], side = 2, at = ages, las = 2, padj = 0)
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
#' @rdname c14_plot
#' @aliases plot,CalibratedAges,missing-method
setMethod("plot", c(x = "CalibratedAges", y = "missing"), plot.CalibratedAges)
