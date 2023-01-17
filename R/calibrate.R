# 14C CALIBRATION
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrate
#' @aliases calibrate,numeric,numeric-method
setMethod(
  f = "calibrate",
  signature = signature(ages = "numeric", errors = "numeric"),
  definition = function(ages, errors, names = NULL, curves = "intcal20",
                        reservoir_offsets = 0, reservoir_errors = 0,
                        from = 55000, to = 0, resolution = 1,
                        normalize = TRUE, F14C = FALSE,
                        drop = TRUE, eps = 1e-05) {
    ## Validation
    n <- length(ages)
    if (is.null(names)) names <- paste0("X", seq_len(n))
    if (length(curves) == 1) curves <- rep(curves, n)
    if (length(reservoir_offsets) == 1) reservoir_offsets <- rep(reservoir_offsets, n)
    if (length(reservoir_errors) == 1) reservoir_errors <- rep(reservoir_errors, n)

    arkhe::assert_missing(ages)
    arkhe::assert_missing(errors)
    arkhe::assert_unique(names)
    arkhe::assert_length(errors, n)
    arkhe::assert_length(names, n)
    arkhe::assert_length(curves, n)
    arkhe::assert_length(reservoir_offsets, n)
    arkhe::assert_length(reservoir_errors, n)

    ## Calibration time range
    calibration_range <- seq(from = from, to = to, by = -resolution)

    ## Calibration curve
    curves <- tolower(curves)
    curve_unique <- unique(curves)
    curve_range <- vector(mode = "list", length = length(curve_unique))
    names(curve_range) <- curve_unique
    for (i in seq_along(curve_unique)) {
      tmp <- get_curve(curve_unique[[i]])

      if (F14C) {
        tmp_f14 <- BP14C_to_F14C(tmp[, 2], tmp[, 3])
        tmp[, 2] <- tmp_f14$ages
        tmp[, 3] <- tmp_f14$errors
      }

      curve_range[[i]] <- list(
        mu = stats::approx(tmp[, 1], tmp[, 2], xout = calibration_range)$y,
        tau = stats::approx(tmp[, 1], tmp[, 3], xout = calibration_range)$y,
        range = calibration_range
      )
    }

    ## Marine reservoir offset
    ages <- ages - reservoir_offsets
    errors <- sqrt(errors^2 + reservoir_errors^2)

    ## Calibrate
    calibrate_fun <- if (F14C) calibrate_F14C else calibrate_BP14C
    dens <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
      dens[[i]] <- calibrate_fun(
        age = ages[i],
        error = errors[i],
        curve_range[[curves[i]]]$mu,
        curve_range[[curves[i]]]$tau,
        eps = eps
      )
    }

    ## Build matrix
    dens <- do.call(rbind, dens)

    if (anyNA(dens)) {
      msg <- "Consider changing the time range to a narrower interval."
      warning(msg, call. = FALSE)
    }

    ## Normalize
    if (normalize) {
      dens <- dens / rowSums(dens, na.rm = TRUE)
      dens[dens < eps] <- 0
      dens <- dens / rowSums(dens, na.rm = TRUE)
    }

    ## Drop
    if (drop) {
      keep <- colSums(dens, na.rm = TRUE) > 0
      dens <- dens[, keep, drop = FALSE]
      from <- max(calibration_range[keep])
    }

    .CalibratedAges(
      dens,
      labels = names,
      ages = ages,
      errors = errors,
      curves = curves,
      start = from,
      resolution = resolution,
      F14C = F14C,
      calendar = "BP"
    )
  }
)

calibrate_BP14C <- function(age, error, mu, tau, eps = 1e-05) {
  tau <- error^2 + tau^2
  dens <- stats::dnorm(age, mean = mu, sd = sqrt(tau))
  dens[dens < eps] <- 0
  dens
}
calibrate_F14C <- function (age, error, calf14, calf14error, eps = 1e-05) {
  f14 <- BP14C_to_F14C(age, error)
  p1 <- (f14$ages - calf14)^2
  p2 <- 2 * (f14$errors^2 + calf14error^2)
  p3 <- sqrt(f14$errors^2 + calf14error^2)
  dens <- exp(-p1 / p2) / p3
  dens[dens < eps] <- 0
  dens
}

#' @export
#' @rdname F14C
#' @aliases BP14C_to_F14C,numeric,numeric-method
setMethod(
  f = "BP14C_to_F14C",
  signature = c(ages = "numeric", errors = "numeric"),
  definition = function(ages, errors) {
    ages <- exp(ages / -8033)
    errors <- ages * errors / 8033
    data.frame(ages = ages, errors = errors)
  }
)

#' @export
#' @rdname F14C
#' @aliases F14C_to_BP14C,numeric,numeric-method
setMethod(
  f = "F14C_to_BP14C",
  signature = c(ratios = "numeric", errors = "numeric"),
  definition = function(ratios, errors) {
    ages <- -8033 * log(ratios)
    errors <- 8033 * errors / ages
    data.frame(ages = ages, errors = errors)
  }
)
