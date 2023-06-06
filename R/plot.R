# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, calendar = getOption("ananke.calendar"),
                                interval = TRUE, level = 0.954,
                                flip = FALSE, ncol = 1,
                                warnings = TRUE, sort = TRUE, decreasing = TRUE,
                                main = NULL, sub = NULL,
                                ann = graphics::par("ann"), axes = TRUE,
                                frame.plot = FALSE,
                                panel.first = NULL, panel.last = NULL,
                                col.density = "grey", col.interval = "#77AADD", ...) {
  ## TODO
  # panel <- match.arg(panel, several.ok = FALSE)
  panel <- "density"

  ## Get data
  n <- NCOL(x)

  ## Graphical parameters
  if (length(col.density) != n) col.density <- rep(col.density, length.out = n)
  if (length(col.interval) != n) col.interval <- rep(col.interval, length.out = n)
  fill.density <- grDevices::adjustcolor(col.density, alpha.f = 0.5)
  fill.interval <- grDevices::adjustcolor(col.interval, alpha.f = 0.5)

  ## Reorder
  if (sort) {
    mid <- median(x)
    k <- order(mid, decreasing = !decreasing)
    x <- x[, k, , drop = FALSE]
    col.density <- col.density[k]
    fill.density <- fill.density[k]
    col.interval <- col.interval[k]
    fill.interval <- fill.interval[k]
  }

  ## Plot
  if (panel == "density") {
    panel_density <- function(x, y, ...) {
      d0 <- which(y > 0) # Keep only density > 0
      lb <- if (min(d0) > 1) min(d0) - 1 else min(d0)
      ub <- if (max(d0) < length(x)) max(d0) + 1 else max(d0)
      xi <- c(x[lb], x[d0], x[ub])
      yi <- c(0, y[d0], 0)

      h <- arkhe::interval_hdr(as.numeric(x), y, level = level)

      graphics::polygon(xi, yi, border = NA, ...)
      graphics::lines(xi, yi, lty = "solid")

      if (isTRUE(interval)) {
        for (i in seq_len(nrow(h))) {
          is_in_h <- xi >= h[i, "start"] & xi <= h[i, "end"]
          graphics::polygon(
            x = c(utils::head(xi[is_in_h], 1), xi[is_in_h], utils::tail(xi[is_in_h], 1)),
            y = c(0, yi[is_in_h], 0),
            border = NA,
            col = fill.interval
          )
        }
        graphics::segments(
          x0 = h[, "start"], x1 = h[, "end"], y0 = 0, y1 = 0,
          lend = 1
        )
        graphics::segments(
          x0 = c(h[, "start"], h[, "end"]), x1 = c(h[, "start"], h[, "end"]),
          y0 = 0, y1 = graphics::par("tcl") * graphics::strheight("M") * -1,
          lend = 1
        )
      }
    }
    methods::callNextMethod(
      x, facet = "multiple",
      calendar = calendar,
      panel = panel_density,
      flip = flip, ncol = ncol,
      main = main, sub = sub, ann = ann, axes = axes,
      frame.plot = frame.plot,
      panel.first = panel.first,
      panel.last = panel.last,
      col = fill.density,
      ...
    )
  }

  if (panel == "interval") stop("TODO", call. = FALSE)

  # if (warnings) {
  #   status <- x@status
  #   graphics::text(x = xlim[1L], y = which(status == 1L), adj = c(0, 0),
  #                  labels = "Date is out of range.", col = "red")
  #   graphics::text(x = xlim[1L], y = which(status == 2L), adj = c(0, 0),
  #                  labels = "Date may extent out of range.", col = "red")
  # }

  invisible(x) # /!\ sorted! /!\
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedAges,missing-method
setMethod("plot", c(x = "CalibratedAges", y = "missing"), plot.CalibratedAges)

#' @export
#' @method plot CalibratedSPD
plot.CalibratedSPD <- function(x, calendar = getOption("ananke.calendar"),
                               main = NULL, sub = NULL,
                               ann = graphics::par("ann"),
                               axes = TRUE, frame.plot = FALSE,
                               panel.first = NULL, panel.last = NULL, ...) {
  n <- NCOL(x)

  ## Graphical parameters
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  lty <- list(...)$lty %||% graphics::par("lty")
  border <- list(...)$border %||% c("black")
  col <- list(...)$col %||% c("grey")
  if (length(col) != n) col <- rep(col, length.out = n)
  col <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- aion::time(x, calendar = NULL)
  xlim <- range(years)
  ylim <- range(x)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  seq_series <- seq_len(n)
  for (i in seq_series) {
    xi <- c(years, rev(years))
    yi <- c(x[, i, drop = TRUE], rep(0, NROW(x)))
    graphics::polygon(xi, yi, border = NA, col = col[i])
    graphics::lines(years, x, lty = lty, lwd = lwd, col = border)
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(x = years, side = 1, format = TRUE, calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- aion::format(calendar)
    ylab <- "Density"
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedSPD,missing-method
setMethod("plot", c(x = "CalibratedSPD", y = "missing"), plot.CalibratedSPD)
