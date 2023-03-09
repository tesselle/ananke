# STATISTICS
#' @include AllGenerics.R
NULL

#' @export
#' @method median CalibratedAges
median.CalibratedAges <- function(x, ...) {
  i <- apply(
    X = x,
    MARGIN = 1,
    FUN = function(x) {
      z <- cumsum(x)
      which.min(abs(z - max(z) / 2))
    }
  )
  years <- get_years(x)
  years[i]
}

#' @export
#' @rdname median
#' @aliases median,CalibratedAges,missing-method
setMethod("median", c(x = "CalibratedAges"), median.CalibratedAges)
