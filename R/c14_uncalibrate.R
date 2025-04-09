# 14C UNCALIBRATION
#' @include AllGenerics.R
NULL

# Uncalibrate ==================================================================
#' @export
#' @rdname c14_uncalibrate
#' @aliases c14_uncalibrate,numeric-method
setMethod(
  f = "c14_uncalibrate",
  signature = c(object = "numeric"),
  definition = function(object, curves = "intcal20") {
    ## Validation
    n <- length(object)
    if (length(curves) != n) curves <- rep(curves, n)
    uncal <- rep(NA_real_, n)

    ## Calibration curve
    curve_data <- c14_curve(tolower(curves))

    for (i in seq_len(n)) {
      z <- stats::approx(
        x = curve_data[[i]][, 1],
        y = curve_data[[i]][, 2],
        xout = object[i],
        rule = 1
      )
      uncal[i] <- z$y
    }
    uncal
  }
)

#' @export
#' @rdname c14_uncalibrate
#' @aliases c14_uncalibrate,CalibratedAges-method
setMethod(
  f = "c14_uncalibrate",
  signature = c(object = "CalibratedAges"),
  definition = function(object, n = 10000, rounding = getOption("ananke.round"),
                        ...) {

    method <- list(...)$method %||% c("L-BFGS-B")

    ## Function to be optimized
    opt_fun <- function(param, curve, samples, ...) {
      cal <- c14_calibrate(
        values = round(param[1]),
        errors = round(param[2]),
        curves = curve
      )
      s1 <- samples
      s2 <- c14_sample(cal, n = n, calendar = BP())

      dens1 <- stats::density(s1, from = min(c(s1, s2)), to = max(c(s1, s2)))$y
      dens2 <- stats::density(s2, from = min(c(s1, s2)), to = max(c(s1, s2)))$y
      dens1 <- dens1 / sum(dens1)
      dens2 <- dens2 / sum(dens2)
      int <- dens1 * log(dens1 / dens2)
      int <- int[dens1 > 0]
      int <- int[!is.infinite(int)]
      sum(int)
    }

    n <- NCOL(object)
    curves <- object@curves

    ## Initial values for the parameters to be optimized over
    spl <- c14_sample(object, n = n, calendar = BP())
    med <- apply(X = spl, MARGIN = 2, FUN = stats::median)
    init_mean <- c14_uncalibrate(med, curves = curves)
    init_sd <- apply(X = spl, MARGIN = 2, FUN = stats::sd)

    opt_mean <- opt_sd <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      cal_spl <- spl[, i]

      opt_run <- stats::optim(
        par = c(init_mean[[i]], init_sd[[i]]),
        fn = opt_fun,
        method = method,
        curve = curves[[i]],
        samples = cal_spl,
        lower = c(-Inf, 1),
        ...
      )
      opt_mean[[i]] <- opt_run$par[1]
      opt_sd[[i]] <- opt_run$par[2]
    }

    ## Rounding
    if (identical(rounding, "stuiver")) {
      opt_mean <- round_values_stuiver(opt_mean)
      opt_sd <- round_errors_stuiver(opt_sd)
    }

    data.frame(mean = opt_mean, sd = opt_sd)
  }
)
