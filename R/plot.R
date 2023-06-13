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

      graphics::polygon(xi, yi, border = NA, ...)
      graphics::lines(xi, yi, lty = "solid")

      if (isTRUE(interval)) {
        h <- arkhe::interval_hdr(as.numeric(x), y, level = level)

        for (i in seq_len(nrow(h))) {
          debut <- h[i, "start"]
          fin <- h[i, "end"]
          if (debut < fin) {
            is_in_h <- xi >= debut & xi <= fin
          } else {
            is_in_h <- xi <= debut & xi >= fin
          }
          xh <- xi[is_in_h]
          yh <- yi[is_in_h]
          graphics::polygon(x = c(xh[1], xh, xh[length(xh)]), y = c(0, yh, 0),
                            border = NA, col = fill.interval)
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
#' @method image RECE
image.RECE <- function(x, calendar = getOption("ananke.calendar"), ...) {
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
    col = col,
    # ylim = ylim,
    xlab = format(calendar), ylab = "Count",
    xaxt = "n", yaxt = "n", ...
  )

  ## Construct axes
  aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                  current_calendar = NULL)
  graphics::axis(side = 2, at = seq_len(max(x)), las = 1)

  invisible(x)
}

#' @export
#' @rdname rec_image
#' @aliases image,RECE,missing-method
setMethod("image", c(x = "RECE"), image.RECE)

#' @export
#' @method plot ProxyRecord
plot.ProxyRecord <- function(x, calendar = getOption("ananke.calendar"),
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
