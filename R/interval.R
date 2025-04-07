# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HDR ==========================================================================
#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,CalibratedAges,missing-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "CalibratedAges", y = "missing"),
  definition = function(x, level = 0.954, ...) {
    ## Check
    c14_validate(x)

    hdr <- apply(
      X = x,
      MARGIN = 2,
      FUN = function(y, x, level, ...) {
        if (all(is.na(y))) return(NULL)
        arkhe::interval_hdr(x, as.numeric(y), level, ...)
      },
      x = aion::time(x, calendar = NULL),
      level = level,
      simplify = FALSE
    )
    n <- vapply(X = hdr, FUN = nrow, FUN.VALUE = integer(1))
    lab <- rep(labels(x), n)

    hdr <- do.call(rbind, hdr)
    .CalibratedIntervals(
      .Id = lab,
      .Start = aion::as_fixed(hdr[, 1]),
      .End = aion::as_fixed(hdr[, 2]),
      p = unname(hdr[, 3])
    )
  }
)

# Credible =====================================================================
#' @export
#' @rdname interval_credible
#' @aliases interval_credible,CalibratedAges,missing-method
setMethod(
  f = "interval_credible",
  signature = c(x = "CalibratedAges"),
  definition = function(x, level = 0.954, n = 100, ...) {
    ## Check
    c14_validate(x)

    spl <- c14_sample(x, n = n, calendar = NULL)
    crd <- apply(
      X = spl,
      MARGIN = 2,
      FUN = interval_credible,
      level = level,
      simplify = FALSE
    )

    crd <- do.call(rbind, crd)
    .CalibratedIntervals(
      .Id = labels(x),
      .Start = aion::as_fixed(crd[, 1]),
      .End = aion::as_fixed(crd[, 2]),
      p = unname(crd[, 3])
    )
  }
)
