# MUTATORS
#' @include AllGenerics.R
NULL

# Get ==========================================================================
#' @export
#' @method labels CalibratedAges
labels.CalibratedAges <- function(object, ...) {
  colnames(object) %||% paste0("X", NCOL(object))
}

#' @export
#' @rdname labels
#' @aliases labels,CalibratedAges-method
setMethod("labels", "CalibratedAges", labels.CalibratedAges)
