# MUTATORS
#' @include AllGenerics.R
NULL

# Get ==========================================================================
#' @export
#' @method labels CalibratedAges
labels.CalibratedAges <- function(object, ...) {
  n <- NCOL(object)
  lab <- colnames(object) %||% paste0("X", seq_len(object))
  lab
}

#' @export
#' @rdname labels
#' @aliases labels,CalibratedAges-method
setMethod("labels", "CalibratedAges", labels.CalibratedAges)
