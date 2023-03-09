# 14C CALIBRATION
#' @include AllGenerics.R
NULL

# Combine ======================================================================
#' @export
#' @rdname c14_combine
#' @aliases c14_combine,numeric,numeric-method
setMethod(
  f = "c14_combine",
  signature = c(ages = "numeric", errors = "numeric"),
  definition = function(ages, errors, groups = NULL) {
    ## Validation
    n <- length(ages)
    if (is.null(groups)) groups <- "X"
    if (length(groups) == 1) groups <- rep(groups, n)
    groups <- factor(x = groups, levels = unique(groups))

    arkhe::assert_missing(ages)
    arkhe::assert_missing(errors)
    arkhe::assert_length(errors, n)
    arkhe::assert_length(groups, n)

    ## Empty groups must be treated as NA
    groups[groups == ""] <- NA

    ## Groups with only one date must be treated as NA
    counts <- table(groups)
    one <- groups %in% names(counts)[counts == 1]

    # NA group will be removed
    # We need to keep isolated dates
    k <- one | is.na(groups)
    solo <- NULL
    if (any(k)) {
      solo <- data.frame(
        groups = as.character(groups[k]),
        ages = ages[k],
        errors = errors[k],
        chi2 = NA_real_,
        p = NA_real_
      )
    }

    combined <- NULL
    if (all(!k)) {
      groups[k] <- NA
      groups <- droplevels(groups)

      ## split() removes NA group
      ages <- split(ages, f = groups)
      errors <- split(errors, f = groups)
      cmbn <- mapply(
        FUN = combine,
        ages = ages,
        errors = errors,
        SIMPLIFY = FALSE
      )
      combined <- data.frame(names(cmbn), do.call(rbind, cmbn))
      colnames(combined) <- c("groups", "ages", "errors", "chi2", "p")
    }

    final <- rbind(solo, combined, make.row.names = FALSE)
    final
  }
)

combine <- function(ages, errors) {
  ## On calcule la moyenne pondérée
  w <- 1 / errors^2 # Facteur de pondération
  moy <- stats::weighted.mean(x = ages, w = w)

  ## On calcule l'incertitude associée à la moyenne pondérée
  err <- sum(1 / errors^2)^(-1 / 2)

  ## On calcule la statistique du test
  chi2 <- sum(((ages - moy) / errors)^2)

  ## On calcule la valeur-p
  p <- 1 - stats::pchisq(chi2, df = length(ages))

  ## On stocke les résultats
  c(moy, err, chi2, p)
}

# Calibrate ====================================================================
#' @export
#' @rdname c14_calibrate
#' @aliases c14_calibrate,numeric,numeric-method
setMethod(
  f = "c14_calibrate",
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
      tmp <- c14_curve(curve_unique[[i]])

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
calibrate_F14C <- function (age, error, mu, tau, eps = 1e-05) {
  f14 <- BP14C_to_F14C(age, error)
  p1 <- (f14$ages - mu)^2
  p2 <- 2 * (f14$errors^2 + tau^2)
  p3 <- sqrt(f14$errors^2 + tau^2)
  dens <- exp(-p1 / p2) / p3
  dens[dens < eps] <- 0
  dens
}

# Calibration curve ============================================================
#' @export
#' @rdname c14_curve
#' @aliases c14_curve,character-method
setMethod(
  f = "c14_curve",
  signature = "character",
  definition = function(x) {
    if (!(x %in% c("intcal20", "intcal13", "intcal09")))
      stop("TODO", call. = FALSE)

    curve_dir <- system.file("curves", package = "ananke")
    curve_path <- file.path(curve_dir, paste0(x, ".14c"))
    curve <- utils::read.table(curve_path, header = FALSE, sep = ",", dec = ".",
                               strip.white = TRUE, comment.char = "#")
    curve <- curve[, c(1, 2, 3)]
    colnames(curve) <- c("CALBP", "14CBP", "sigma")
    curve
  }
)

# F14C <> BP14C ================================================================
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
