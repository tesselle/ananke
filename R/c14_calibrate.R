# 14C CALIBRATION
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_calibrate
#' @aliases c14_calibrate,numeric,numeric-method
setMethod(
  f = "c14_calibrate",
  signature = signature(values = "numeric", errors = "numeric"),
  definition = function(values, errors, names = NULL, curves = "intcal20",
                        reservoir_offsets = 0, reservoir_errors = 0,
                        from = 55000, to = 0, resolution = 1,
                        normalize = TRUE, F14C = FALSE,
                        drop = TRUE, eps = 1e-06,
                        verbose = getOption("ananke.verbose")) {
    ## Validation
    n <- length(values)
    if (is.null(names)) names <- paste0("X", seq_len(n))
    if (length(curves) == 1)
      curves <- rep(curves, n)
    if (length(reservoir_offsets) == 1)
      reservoir_offsets <- rep(reservoir_offsets, n)
    if (length(reservoir_errors) == 1)
      reservoir_errors <- rep(reservoir_errors, n)

    arkhe::assert_missing(values)
    arkhe::assert_missing(errors)
    arkhe::assert_unique(names)
    arkhe::assert_length(errors, n)
    arkhe::assert_length(names, n)
    arkhe::assert_length(curves, n)
    arkhe::assert_length(reservoir_offsets, n)
    arkhe::assert_length(reservoir_errors, n)

    ## Calibration time range
    cal_range <- seq(from = from, to = to, by = -resolution)

    ## Calibration curve
    curves <- tolower(curves)
    curve_range <- approx_curve(unique(curves), out = cal_range, F14C = F14C)

    ## Marine reservoir offset
    values <- values - reservoir_offsets
    errors <- sqrt(errors^2 + reservoir_errors^2)

    ## Calibrate
    calibrate_fun <- if (F14C) calibrate_F14C else calibrate_BP14C
    dens <- vector(mode = "list", length = n)
    status <- integer(n)
    for (i in seq_len(n)) {
      d <- calibrate_fun(
        x = values[i],
        error = errors[i],
        mu = curve_range[[curves[i]]]$mu,
        tau = curve_range[[curves[i]]]$tau
      )

      ## Check
      if (anyNA(d)) {
        d[is.na(d)] <- 0
        msg <- "Consider changing the calibration time range to a narrower interval."
        if (verbose) message(msg)
      }

      max_cal <- curve_range[[curves[i]]]$max
      min_cal <- curve_range[[curves[i]]]$min

      if (values[[i]] >= max_cal || values[[i]] <= min_cal) {
        ## L'age à calibrer est hors de l'étendue de la courbe de calibration
        if (verbose) warning(print_out(names[[i]], maybe = FALSE), call. = FALSE)
        status[i] <- 1L
      } else if (d[1] > eps || d[length(d)] > eps) {
        ## L'age calibré est en partie hors de l'étendue de la courbe
        if (verbose) warning(print_out(names[[i]], maybe = TRUE), call. = FALSE)
        status[i] <- 2L
      }

      d[d < eps] <- 0
      dens[[i]] <- d
    }

    ## Build matrix
    dens <- do.call(rbind, dens)

    ## Normalize
    if (F14C & !normalize) normalize <- TRUE
    if (normalize) {
      dens <- dens / rowSums(dens, na.rm = TRUE)
      dens[dens < eps] <- 0
      dens <- dens / rowSums(dens, na.rm = TRUE)
    }

    ## Drop
    if (drop) {
      keep_zero <- colSums(dens, na.rm = TRUE) > 0
      keep_from <- which.max(keep_zero) # First TRUE
      keep_to <- length(keep_zero) - which.max(rev(keep_zero)) + 1 # Last TRUE
      keep <- seq(from = keep_from, to = keep_to, by = 1)
      dens <- dens[, keep, drop = FALSE]
      cal_range <- cal_range[keep]
    }

    time_series <- aion::series(
      object = t(dens),
      time = cal_range,
      calendar = BP(),
      names = names
    )
    .CalibratedAges(
      time_series,
      values = values,
      errors = errors,
      curves = curves,
      F14C = F14C,
      status = status
    )
  }
)

calibrate_BP14C <- function(x, error, mu, tau) {
  tau <- error^2 + tau^2
  dens <- stats::dnorm(x, mean = mu, sd = sqrt(tau))
  dens
}
calibrate_F14C <- function(x, error, mu, tau) {
  p1 <- (x - mu)^2
  p2 <- 2 * (error^2 + tau^2)
  p3 <- sqrt(error^2 + tau^2)
  dens <- exp(-p1 / p2) / p3
  dens
}
