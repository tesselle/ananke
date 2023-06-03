# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, calendar = getOption("ananke.calendar"),
                                density = TRUE, interval = TRUE, level = 0.954,
                                warnings = TRUE, sort = TRUE, decreasing = TRUE,
                                main = NULL, sub = NULL,
                                ann = graphics::par("ann"), axes = TRUE,
                                frame.plot = FALSE,
                                panel.first = NULL, panel.last = NULL,
                                col.density = "grey", col.interval = "#77AADD", ...) {
  ## Get data
  n_dates <- NCOL(x)

  ## Graphical parameters
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  tcl <- list(...)$tcl %||% graphics::par("tcl")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  if (length(col.density) != n_dates)
    col.density <- rep(col.density, length.out = n_dates)
  if (length(col.interval) != n_dates)
    col.interval <- rep(col.interval, length.out = n_dates)
  fill.density <- grDevices::adjustcolor(col.density, alpha.f = 0.5)
  fill.interval <- grDevices::adjustcolor(col.interval, alpha.f = 0.5)

  ## Save and restore
  mar <- graphics::par("mar")
  mar[2] <- inch2line(colnames(x), cex = cex.axis) + 0.5
  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- x@time
  xlim <- range(years)
  ylim <- c(1, n_dates + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder
  if (sort) {
    mid <- median(x)
    k <- order(mid, decreasing = decreasing)
    x <- x[, k, drop = FALSE]
    col.density <- col.density[k]
    fill.density <- fill.density[k]
    col.interval <- col.interval[k]
    fill.interval <- fill.interval[k]
  }

  ## Plot
  ages <- seq_len(n_dates)
  interval_draw <- FALSE
  if (!density && !interval) density <- TRUE
  if (interval && !is.null(level)) {
    hdr <- interval_hdr(x, level = level, calendar = NULL)
    interval_draw <- TRUE
  }

  if (density) {
    for (i in ages) {
      d <- x[, i, drop = TRUE]

      d <- (d - min(d)) / max(d - min(d)) * 1.5
      d0 <- which(d > 0) # Keep only density > 0
      lb <- if (min(d0) > 1) min(d0) - 1 else min(d0)
      ub <- if (max(d0) < length(years)) max(d0) + 1 else max(d0)
      xi <- c(years[lb], years[d0], years[ub])
      yi <- c(0, d[d0], 0) + i

      graphics::polygon(xi, yi, border = NA, col = fill.density[i])

      if (interval_draw) {
        h <- hdr[[i]]
        for (j in seq_len(nrow(h))) {
          is_in_h <- xi >= h[j, "start"] & xi <= h[j, "end"]
          graphics::polygon(
            x = c(utils::head(xi[is_in_h], 1), xi[is_in_h], utils::tail(xi[is_in_h], 1)),
            y = c(i, yi[is_in_h], i),
            border = NA, col = fill.interval[i]
          )
        }
      }

      graphics::lines(xi, yi, lty = "solid", col = "black")
    }
  }
  if (interval_draw) {
    for (i in ages) {
      h <- hdr[[i]]
      graphics::segments(
        x0 = h[, "start"], x1 = h[, "end"], y0 = i, y1 = i,
        col = if (density) "black" else col.interval[i],
        lty = lty, lwd = lwd, lend = 1
      )
      graphics::segments(
        x0 = c(h[, "start"], h[, "end"]), x1 = c(h[, "start"], h[, "end"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = if (density) "black" else col.interval[i],
        lty = lty, lwd = lwd, lend = 1
      )
    }
  }
  if (warnings) {
    status <- x@status
    graphics::text(x = xlim[1L], y = which(status == 1L), adj = c(0, 0),
                   labels = "Date is out of range.", col = "red")
    graphics::text(x = xlim[1L], y = which(status == 2L), adj = c(0, 0),
                   labels = "Date may extent out of range.", col = "red")
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::axis_year(x = years, side = 1, format = TRUE, calendar = calendar)
    graphics::mtext(colnames(x)[ages], side = 2, at = ages, las = 2, padj = 0,
                    cex.axis = cex.axis, col.axis = col.axis, font.axis = font.axis)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- aion::format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
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
  years <- x@time
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
    aion::axis_year(x = years, side = 1, format = TRUE, calendar = calendar)
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
