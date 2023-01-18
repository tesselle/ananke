# MUTATORS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname mutators
#' @aliases names,CalibratedAges-method
setMethod(
  f = "names",
  signature = "CalibratedAges",
  definition = function(x) x@labels
)

#' @export
#' @rdname mutators
#' @aliases names<-,CalibratedAges-method
setMethod(
  f = "names<-",
  signature = "CalibratedAges",
  definition = function(x, value) {
    x@events <- value
    methods::validObject(x)
    x
  }
)

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_calendar,CalibratedAges-method
setMethod(
  f = "get_calendar",
  signature = "CalibratedAges",
  definition = function(x) x@calendar
)
