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
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    time <- x@.Time
    values <- x@values
    errors <- x@errors
    curves <- x@curves
    reservoir_offsets <- x@reservoir_offsets
    reservoir_errors <- x@reservoir_errors
    positions <- x@positions
    status <- x@status

    z <- z[i, j, k, drop = drop]
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      time <- time[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, dimnames(x)[2L])
      values <- values[j]
      errors <- errors[j]
      curves <- curves[j]
      reservoir_offsets <- reservoir_offsets[j]
      reservoir_errors <- reservoir_errors[j]
      positions <- positions[j]
      status <- status[j]
    }

    if (isTRUE(drop)) return(z)
    methods::initialize(x, z, .Time = time, values = values, errors = errors,
                        curves = curves, reservoir_offsets = reservoir_offsets,
                        reservoir_errors = reservoir_errors,
                        positions = positions, status = status)
  }
)
