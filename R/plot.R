# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, calendar = get_calendar(),
                                density = TRUE, interval = TRUE, level = 0.954,
                                decreasing = TRUE,
                                col.density = "grey", col.interval = "#77AADD",
                                main = NULL, sub = NULL,
                                axes = TRUE, frame.plot = FALSE,
                                ann = graphics::par("ann"),
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Check
  c14_validate(x)

  ## Graphical parameters
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  if (length(col.density) == 1)
    col.density <- rep(col.density, length.out = NCOL(x))
  if (length(col.interval) == 1)
    col.interval <- rep(col.interval, length.out = NCOL(x))
  fill.density <- grDevices::adjustcolor(col.density, alpha.f = 0.5)
  fill.interval <- grDevices::adjustcolor(col.interval, alpha.f = 0.5)

  ## Clean
  out <- x@status == 1L # Out of calibration range
  x <- x[, !out, , drop = FALSE]
  col.density <- col.density[!out]
  fill.density <- fill.density[!out]
  col.interval <- col.interval[!out]
  fill.interval <- fill.interval[!out]

  ## Reorder
  k <- order(x@positions, decreasing = decreasing)
  x <- x[, k, , drop = FALSE]
  lab <- labels(x)
  col.density <- col.density[k]
  fill.density <- fill.density[k]
  col.interval <- col.interval[k]
  fill.interval <- fill.interval[k]

  ## Compute interval
  hdr <- interval_hdr(x, level = level)
  hdr <- as.list(hdr, calendar = calendar)

  ## Save and restore
  mar <- graphics::par("mar")
  mar[2] <- inch2line(lab, cex = cex.axis) + 0.5
  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  n <- NCOL(x)
  xlim <- range(aion::time(x, calendar = NULL))
  ylim <- c(1, n + 1)
  if (!is.null(calendar)) xlim <- aion::as_year(xlim, calendar = calendar)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  years <- aion::time(x, calendar = calendar)
  tick_height <- graphics::par("tcl") * graphics::strheight("M") * -1
  if (isFALSE(density)) tick_height <- 0

  for (i in seq_len(n)) {
    y <- x[, i, k = 1, drop = TRUE]
    y <- (y - min(y)) / max(y - min(y)) * 0.9
    if (isTRUE(density)) {
      d0 <- which(y > 0) # Keep only density > 0
      lb <- if (min(d0) > 1) min(d0) - 1 else min(d0)
      ub <- if (max(d0) < length(years)) max(d0) + 1 else max(d0)
      xi <- c(years[lb], years[d0], years[ub])
      yi <- c(i, y[d0] + i, i)

      graphics::polygon(xi, yi, border = NA, col = fill.density[i])
      graphics::lines(xi, yi, lty = "solid")
    }

    if (isTRUE(interval)) {
      h <- hdr[[i]]

      if (isTRUE(density)) {
        for (j in seq_len(nrow(h))) {
          debut <- h[j, "start"]
          fin <- h[j, "end"]
          if (debut < fin) is_in_h <- xi >= debut & xi <= fin
          else is_in_h <- xi <= debut & xi >= fin
          xh <- xi[is_in_h]
          yh <- yi[is_in_h]
          graphics::polygon(x = c(xh[1], xh, xh[length(xh)]),
                            y = c(i, yh, i),
                            border = NA, col = fill.interval[i])
        }
      }

      params <- list(x0 = h[, "start"], x1 = h[, "end"],
                     y0 = i, y1 = i, lend = 1)
      dots <- list(...)
      dots <- utils::modifyList(dots, params)
      if (isFALSE(density)) dots$col <- col.interval[i]
      do.call(graphics::segments, dots)
      graphics::segments(x0 = c(h[, "start"], h[, "end"]),
                         x1 = c(h[, "start"], h[, "end"]),
                         y0 = i, y1 = i + tick_height, lend = 1,
                         ...)
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::axis(side = 2, at = seq_len(n), labels = lab, las = 2,
                   lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else format(calendar)
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = NULL)
  }

  invisible(x)
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedAges,missing-method
setMethod("plot", c(x = "CalibratedAges", y = "missing"), plot.CalibratedAges)

## SPD =========================================================================
#' @export
#' @method plot CalibratedSPD
plot.CalibratedSPD <- function(x, calendar = get_calendar(),
                               main = NULL, sub = NULL,
                               ann = graphics::par("ann"),
                               axes = TRUE, frame.plot = FALSE,
                               panel.first = NULL, panel.last = NULL, ...) {
  n <- NCOL(x)

  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  if (length(col) != n) col <- rep(col, length.out = n)
  col <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Plot
  panel_density <- function(x, y, ...) {
    graphics::polygon(x = c(x, rev(x)), y = c(y, rep(0, length(y))),
                      border = NA, ...)
    graphics::lines(x, y, col = "black")
  }

  methods::callNextMethod(
    x, facet = "multiple",
    calendar = calendar,
    panel = panel_density,
    main = main, sub = sub, ann = ann, axes = axes,
    frame.plot = frame.plot,
    panel.first = panel.first,
    panel.last = panel.last,
    col = col,
    ...
  )

  invisible(x)
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedSPD,missing-method
setMethod("plot", c(x = "CalibratedSPD", y = "missing"), plot.CalibratedSPD)

#' @export
#' @method plot RECE
plot.RECE <- function(x, calendar = get_calendar(), ...) {
  ## Binary array
  bin <- array(FALSE, dim = c(nrow(x), max(x), ncol(x)))
  for (j in seq_len(ncol(x))) {
    z <- x[, j, , drop = TRUE]
    for (i in seq_along(z)) {
      bin[i, z[i], j] <- z[i] > 0
    }
  }
  bin <- apply(X = bin, MARGIN = c(1, 2), FUN = sum)
  bin[bin == 0] <- NA

  ## Add annotation
  years <- aion::time(x, calendar = NULL)

  ## Plot
  graphics::image(
    x = years,
    y = seq_len(max(x)),
    z = log(bin),
    xlab = format(calendar),
    ylab = "Count",
    xaxt = "n",
    yaxt = "n",
    ...
  )

  ## Construct axes
  aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                  current_calendar = NULL)
  graphics::axis(side = 2, at = seq_len(max(x)), las = 1)

  invisible(x)
}

#' @export
#' @rdname rec_plot
#' @aliases plot,RECE,missing-method
setMethod("plot", c(x = "RECE", y = "missing"), plot.RECE)

#' @export
#' @method plot ProxyRecord
plot.ProxyRecord <- function(x, calendar = get_calendar(),
                             iqr = TRUE,
                             xlab = NULL, ylab = NULL,
                             col = grDevices::hcl.colors(12, "YlOrRd", rev = TRUE),
                             col.mean = "black", col.iqr = col.mean,
                             lty.mean = 1, lty.iqr = 3,
                             lwd.mean = 2, lwd.iqr = lwd.mean, ...) {
  ## Get data
  years <- aion::time(x, calendar = NULL)
  z <- apply(
    X = x@density,
    MARGIN = 1,
    FUN = function(d) (d - min(d)) / max(d - min(d))
  )
  z[z == 0] <- NA

  ## Plot
  graphics::image(
    x = years,
    y = x@proxy,
    z = t(z),
    col = col,
    xaxt = "n",
    yaxt = "n",
    xlab = xlab %||% format(calendar),
    ylab = ylab %||% "Proxy",
    ...
  )

  ## Construct axes
  aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                  current_calendar = NULL)
  graphics::axis(side = 2, las = 1)

  if (isTRUE(iqr)) {
    m <- mean(x)
    graphics::lines(x = years, y = m, col = col.mean,
                    lty = lty.mean, lwd = lwd.mean)

    q <- quantile(x, probs = c(0.25, 0.75))
    graphics::lines(x = years, y = q[, 1], col = col.iqr,
                    lty = lty.iqr, lwd = lwd.iqr)
    graphics::lines(x = years, y = q[, 2], col = col.iqr,
                    lty = lty.iqr, lwd = lwd.iqr)
  }

  invisible(x)
}

#' @export
#' @rdname proxy_plot
#' @aliases plot,ProxyRecord,missing-method
setMethod("plot", c(x = "ProxyRecord", y = "missing"), plot.ProxyRecord)
