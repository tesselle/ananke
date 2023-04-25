# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HPDI =========================================================================
#' HPD Regions
#'
#' @param object A [`CalibratedAges-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @return
#'  A [`list`] of `numeric` [`matrix`] giving the lower and upper boundaries of
#'  the HPD interval and associated probabilities.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-14c-hpdi.R
#' @author N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases hpdi,CalibratedAges,missing-method
#' @export
setMethod(
  f = "hpdi",
  signature = c(object = "CalibratedAges", density = "missing"),
  definition = function(object, level = 0.954) {
    hpd <- apply(
      X = object,
      MARGIN = 2,
      FUN = function(x, years, level) hpdi(years, density = x, level = level),
      years = time(object),
      level = level,
      simplify = FALSE
    )

    names(hpd) <- names(object)
    hpd
  }
)
