# STATISTICS
#' @include AllGenerics.R
NULL

# Quantile =====================================================================
#' Quantiles of a Density Estimate
#'
#' @param x A [`numeric`] vector giving the n coordinates of the points where
#'  the density is estimated.
#' @param y A `numeric` [`matrix`] of density estimates (each column is a
#'  density estimate).
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]}.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Currently not used.
#' @return
#'  A `numeric` [`matrix`] containing the quantiles.
#' @keywords internal
#' @noRd
quantile_density <- function(x, y, probs = seq(0, 1, 0.25), na.rm = FALSE, ...) {
  eps <- 100 * .Machine$double.eps
  if (anyNA(probs) | any(probs < -eps | probs > 1 + eps))
    stop(sprintf("%s outside [0,1]", sQuote("probs")))

  q <- apply(
    X = y,
    MARGIN = 2,
    FUN = function(y, x, probs, na.rm) {
      np <- length(probs)
      qs <- rep(NA_real_, np)
      if (na.rm) {
        i <- !is.na(x) & !is.na(y)
        x <- x[i]
        y <- y[i]
      }
      if (np > 0) {
        nn <- length(x)
        Fx <- cumsum(y * c(0, diff(x)))
        Fx <- Fx / Fx[nn]
        for (j in seq_len(np)) {
          ii <- min(which(Fx >= probs[j]))
          if (!is.na(ii) && ii >= 1 && ii <= nn) qs[j] <- x[ii]
        }
        qs
      }
    },
    x = x,
    probs = probs,
    na.rm = na.rm
  )

  if (!is.null(dim(q))) {
    q <- t(q)
    colnames(q) <- paste0(round(probs * 100, digits = 0), "%")
  }
  q
}

#' @export
#' @method quantile CalibratedAges
quantile.CalibratedAges <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                                    ..., calendar = get_calendar()) {
  quantile_density(x = aion::time(x, calendar = calendar), y = x,
                   probs = probs, na.rm = na.rm, ...)
}

#' @export
#' @rdname quantile
#' @aliases quantile,CalibratedAges,missing-method
setMethod("quantile", c(x = "CalibratedAges"), quantile.CalibratedAges)

#' @export
#' @method quantile ProxyRecord
quantile.ProxyRecord <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...) {
  quantile_density(x = x@proxy, y = t(x@density),
                   probs = probs, na.rm = na.rm, ...)
}

#' @export
#' @rdname quantile
#' @aliases quantile,ProxyRecord,missing-method
setMethod("quantile", c(x = "ProxyRecord"), quantile.ProxyRecord)

# Median =======================================================================
#' @export
#' @method median CalibratedAges
median.CalibratedAges <- function(x, na.rm = FALSE, ...,
                                  calendar = get_calendar()) {
  quantile(x, probs = 0.5, na.rm = na.rm, ..., calendar = calendar)
}

#' @export
#' @rdname median
#' @aliases median,CalibratedAges,missing-method
setMethod("median", c(x = "CalibratedAges"), median.CalibratedAges)

# Mean =========================================================================
#' Mean of a Density Estimate
#'
#' @param x A [`numeric`] vector giving the n coordinates of the points where
#'  the density is estimated.
#' @param y A `numeric` [`matrix`] of density estimates (each column is a
#'  density estimate).
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
mean_density <- function(x, y, na.rm = FALSE, ...) {
  apply(
    X = y,
    MARGIN = 2,
    FUN = function(w, x, na.rm) {
      if (na.rm) {
        i <- !is.na(w) & !is.na(x)
        x <- x[i]
        w <- w[i]
      }
      stats::weighted.mean(x = x, w = w)
    },
    x = x,
    na.rm = na.rm
  )
}

#' @export
#' @method mean CalibratedAges
mean.CalibratedAges <- function(x, na.rm = FALSE, ...,
                                calendar = get_calendar()) {
  mean_density(x = aion::time(x, calendar = calendar), y = x,
               calendar = calendar)
}

#' @export
#' @rdname mean
#' @aliases mean,CalibratedAges,missing-method
setMethod("mean", c(x = "CalibratedAges"), mean.CalibratedAges)

#' @export
#' @method mean ProxyRecord
mean.ProxyRecord <- function(x, na.rm = FALSE, ...) {
  mean_density(x = x@proxy, y = t(x@density), na.rm = na.rm, ...)
}

#' @export
#' @rdname mean
#' @aliases mean,ProxyRecord,missing-method
setMethod("mean", c(x = "ProxyRecord"), mean.ProxyRecord)
