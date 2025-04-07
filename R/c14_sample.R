# SAMPLE 14C
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_sample
#' @aliases c14_sample,CalibratedAges-method
setMethod(
  f = "c14_sample",
  signature = "CalibratedAges",
  definition = function(object, n = 100, calendar = get_calendar()) {
    x <- aion::time(object, calendar = calendar)
    apply(
      X = object,
      MARGIN = 2,
      FUN = function(p) sample(x, size = n, replace = TRUE, prob = p)
    )
  }
)
