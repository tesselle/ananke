# SUBSET
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,CalibratedAges-method
setMethod(
  f = "[",
  signature = c(x = "CalibratedAges"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod() # Method for `TimeSeries`

    if (is.null(dim(z)) || isTRUE(drop)) return(z)

    ages <- x@ages
    errors <- x@errors
    curves <- x@curves
    status <- x@status
    if (!missing(j)) {
      ages <- ages[j]
      errors <- errors[j]
      curves <- curves[j]
      status <- status[j]
    }
    methods::initialize(x, z, ages = ages, errors = errors,
                        curves = curves, status = status)
  }
)
