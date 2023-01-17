# MUTATORS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname mutators
#' @aliases names,CalibratedAges-method
setMethod(
  f = "names",
  signature = "CalibratedAges",
  definition = function(x) x@events
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
#' @rdname get_curve
#' @aliases get_curve,character-method
setMethod(
  f = "get_curve",
  signature = "character",
  definition = function(x) {
    curve_dir <- system.file("curves", package = "ananke")
    curve_path <- file.path(curve_dir, paste0(x, ".14c"))
    curve <- utils::read.table(curve_path, header = FALSE, sep = ",", dec = ".",
                               strip.white = TRUE, comment.char = "#")
    curve <- curve[, c(1, 2, 3)]
    colnames(curve) <- c("CALBP", "14CBP", "sigma")
    curve
  }
)

#' @export
#' @rdname mutators
#' @aliases get_calendar,CalibratedAges-method
setMethod(
  f = "get_calendar",
  signature = "CalibratedAges",
  definition = function(x) x@calendar
)
