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
    if (!all(k)) {
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
                        drop = TRUE, eps = 1e-06) {
    ## Validation
    n <- length(ages)
    if (is.null(names)) names <- paste0("X", seq_len(n))
    if (length(curves) != n) curves <- rep(curves, n)
    if (length(reservoir_offsets) != n) reservoir_offsets <- rep(reservoir_offsets, n)
    if (length(reservoir_errors) != n ) reservoir_errors <- rep(reservoir_errors, n)

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
    curve_data <- c14_curve(unique(curves))
    curve_range <- lapply(
      X = curve_data,
      FUN = function(x, xout) {
        if (F14C) {
          x_f14 <- BP14C_to_F14C(x[, 2], x[, 3])
          x[, 2] <- x_f14$ratio
          x[, 3] <- x_f14$error
        }

        list(
          mu = stats::approx(x[, 1], x[, 2], xout = xout)$y,
          tau = stats::approx(x[, 1], x[, 3], xout = xout)$y,
          max_cal = max(x[, 1]),
          max_age = max(x[, 2]),
          min_age = min(x[, 2])
        )
      },
      xout = calibration_range
    )

    ## Marine reservoir offset
    ages <- ages - reservoir_offsets
    errors <- sqrt(errors^2 + reservoir_errors^2)

    ## Calibrate
    calibrate_fun <- if (F14C) calibrate_F14C else calibrate_BP14C
    dens <- vector(mode = "list", length = n)
    status <- integer(n)
    for (i in seq_len(n)) {
      d <- calibrate_fun(
        x = ages[i],
        error = errors[i],
        mu = curve_range[[curves[i]]]$mu,
        tau = curve_range[[curves[i]]]$tau
      )

      ## Check
      if (anyNA(d)) {
        d[is.na(d)] <- 0
        message("Consider changing the time range to a narrower interval.")
      }

      max_age <- curve_range[[curves[i]]]$max_age
      min_age <- curve_range[[curves[i]]]$min_age
      if (ages[i] >= max_age || ages[i] <= min_age) {
        status[i] <- 1L
      } else if (d[1] > eps || d[length(d)] > eps) {
        status[i] <- 2L
      }

      d[d < eps] <- 0
      dens[[i]] <- d
    }

    ## Build matrix
    dens <- do.call(rbind, dens)

    ## Check
    calibrate_check(names, status)

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
      calibration_range <- calibration_range[keep]
    }

    time_series <- aion::series(
      object = t(dens),
      time = calibration_range,
      calendar = BP(),
      names = names
    )
    .CalibratedAges(
      time_series,
      ages = ages,
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
calibrate_F14C <- function(x, error, mu, tau, convert = FALSE) {
  if (convert) {
    z <- BP14C_to_F14C(x, error)
  } else {
    z <- data.frame(ratio = x, error = error)
  }
  p1 <- (z$ratio - mu)^2
  p2 <- 2 * (z$error^2 + tau^2)
  p3 <- sqrt(z$error^2 + tau^2)
  dens <- exp(-p1 / p2) / p3
  dens
}

calibrate_check <- function(names, status) {
  if (any(status == 1L)) {
    is_out <- which(status == 1L)
    warn <- sprintf("Date %s is out of range.", dQuote(names[is_out]))
    for (w in warn) warning(w, call. = FALSE)
  }
  if (any(status == 2L)) {
    is_out <- which(status == 2L)
    warn <- sprintf("Date %s may extent out of range.", dQuote(names[is_out]))
    for (w in warn) warning(w, call. = FALSE)
  }
}

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
    curve_data <- c14_curve(unique(tolower(curves)))

    for (i in seq_len(n)) {
      uncal[i] <- stats::approx(
        x = curve_data[[i]][, 1],
        y = curve_data[[i]][, 2],
        xout = object[i],
        rule = 1)$y
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
  definition = function(object, ...) {

    method <- list(...)$method %||% c("L-BFGS-B")

    ## Function to be optimized
    opt_fun <- function(param, curve, samples, ...) {
      cal <- c14_calibrate(
        ages = round(param[1]),
        errors = round(param[2]),
        curves = curve
      )
      s1 <- samples
      s2 <- c14_sample(cal)

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
    spl <- c14_sample(object)
    curves <- object@curves
    opt_mean <- opt_sd <- numeric(n)
    for (i in seq_len(n)) {
      cal_spl <- spl[, i]
      init_mean <- methods::callGeneric(
        object = stats::median(cal_spl),
        curves = curves[[i]]
      )
      init_sd <- stats::sd(cal_spl)

      opt_run <- stats::optim(
        par = c(init_mean, init_sd),
        fn = opt_fun,
        method = method,
        curve = curves[[i]],
        samples = cal_spl,
        lower = c(-Inf, 1),
        ...
      )
      opt_mean[[i]] <- round(opt_run$par[1])
      opt_sd[[i]] <- round(opt_run$par[2])
    }

    data.frame(mean = opt_mean, sd = opt_sd)
  }
)

c14_sample <- function(object, n = 10000, calendar = BP()) {
  apply(
    X = object,
    MARGIN = 2,
    FUN = function(prob, size, x) {
      sample(x, size = size, replace = TRUE, prob = prob)
    },
    x = aion::time(object, calendar = calendar),
    size = n
  )
}

# Calibration curve ============================================================
#' @export
#' @rdname c14_curve
#' @aliases c14_curve,character-method
setMethod(
  f = "c14_curve",
  signature = c(object = "character"),
  definition = function(object) {
    curve_ok <- c("intcal20", "intcal13", "intcal09", "intcal04",
                  "marine20", "marine13", "marine09", "marine04")
    object <- match.arg(object, choices = curve_ok, several.ok = TRUE)

    curves <- lapply(
      X = object,
      FUN = function(x) {
        curve_dir <- system.file("extdata", package = "ananke")
        curve_path <- file.path(curve_dir, paste0(x, ".14c"))
        curve <- utils::read.table(curve_path, header = FALSE, sep = ",",
                                   dec = ".", strip.white = TRUE,
                                   comment.char = "#")
        curve <- curve[, c(1, 2, 3)]
        colnames(curve) <- c("CALBP", "AGE", "ERROR")
        curve
      }
    )
    names(curves) <- object
    curves
  }
)

#' @export
#' @rdname c14_curve
#' @aliases c14_curve,.CalibratedAges-method
setMethod(
  f = "c14_curve",
  signature = c(object = "CalibratedAges"),
  definition = function(object) {
    methods::callGeneric(object@curves)
  }
)

# F14C <> BP14C ================================================================
#' @export
#' @rdname F14C
#' @aliases BP14C_to_F14C,numeric,numeric-method
setMethod(
  f = "BP14C_to_F14C",
  signature = c(ages = "numeric", errors = "numeric"),
  definition = function(ages, errors, lambda = 8033) {
    ratios <- exp(ages / -lambda)
    sigma <- ratios * errors / lambda
    data.frame(ratio = ratios, error = sigma)
  }
)

#' @export
#' @rdname F14C
#' @aliases F14C_to_BP14C,numeric,numeric-method
setMethod(
  f = "F14C_to_BP14C",
  signature = c(ratios = "numeric", errors = "numeric"),
  definition = function(ratios, errors, lambda = 8033) {
    ages <- -lambda * log(ratios)
    sigma <- lambda * errors / ratios
    data.frame(age = ages, error = sigma)
  }
)

# REC ==========================================================================
#' @export
#' @rdname c14_ensemble
#' @aliases c14_ensemble,CalibratedAges-method
setMethod(
  f = "c14_ensemble",
  signature = "CalibratedAges",
  definition = function(object, from = NULL, to = NULL, by = 10, n = 100,
                        calendar = BP(), progress = getOption("ananke.progress")) {
    ## Check
    calibrate_check(colnames(object), object@status)

    ## Get data
    rd <- aion::time(object, calendar = NULL)
    if (is.null(from)) from <- aion::start(object, calendar = calendar)
    if (is.null(to)) to <- aion::end(object, calendar = calendar)
    grid_years <- seq(from = from, to = to, by = by * sign(to - from))
    grid_rd <- aion::fixed(grid_years, calendar = calendar)

    ## Align 14C date densities onto the grid
    c14_dens <- object[, , 1, drop = TRUE]
    c14_aligned <- apply(
      X = c14_dens,
      MARGIN = 2,
      FUN = function(y, x, grid) {
        fun <- stats::approxfun(x = x, y = y)
        fun(grid)
      },
      x = rd,
      grid = grid_rd
    )
    c14_aligned[is.na(c14_aligned)] <- 0

    ## Build matrix to store the RECE
    count <- matrix(data = 0, nrow = n, ncol = length(grid_rd))
    colnames(count) <- grid_rd

    progress_bar <- progress
    if (progress_bar) pbar <- utils::txtProgressBar(max = n, style = 3)

    n_seq <- seq_len(n)
    for (i in n_seq) {
      ## Sample
      spl <- apply(
        X = c14_aligned,
        MARGIN = 2,
        FUN = function(x, grid) {
          if (sum(x) == 0) return(NA)
          sample(grid, size = 1, prob = x)
        },
        grid = grid_rd
      )
      ## Count
      tbl <- unclass(table(spl)) # Named integer vector
      count[i, names(tbl)] <- tbl
      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }
    count[is.na(count)] <- 0
    if (progress_bar) close(pbar)

    ## Return an RECE object
    ts <- aion::series(
      object = t(count),
      time = grid_rd
      # names = colnames(object)
    )
    .RECE(ts)
  }
)

# SPD ==========================================================================
#' @export
#' @rdname c14_spd
#' @aliases c14_spd,CalibratedAges-method
setMethod(
  f = "c14_spd",
  signature = "CalibratedAges",
  definition = function(object, normalize_date = FALSE, normalize_spd = FALSE) {
    ## Check
    calibrate_check(colnames(object), object@status)

    dens <- t(object[, , 1, drop = TRUE])
    if (normalize_date) dens <- dens / rowSums(dens, na.rm = TRUE)
    spd <- colSums(dens, na.rm = TRUE)
    if (normalize_spd) spd <- spd / sum(spd, na.rm = TRUE)

    time_series <- aion::series(
      object = spd,
      time = aion::time(object, calendar = NULL)
    )
    .CalibratedSPD(time_series)
  }
)
