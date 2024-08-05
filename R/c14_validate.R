# 14C CALIBRATION CHECK
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_validate
#' @aliases c14_validate,CalibratedAges-method
setMethod(
  f = "c14_validate",
  signature = signature(object = "CalibratedAges"),
  definition = function(object) {
    status <- object@status
    lab <- labels(object)

    if (isTRUE(getOption("ananke.verbose"))) {
      if (any(status == 1L)) {
        is_out <- which(status == 1L)
        warn <- print_out(lab[is_out], maybe = FALSE)
        for (w in warn) warning(w, call. = FALSE)
      }
      if (any(status == 2L)) {
        is_out <- which(status == 2L)
        warn <- print_out(lab[is_out], maybe = TRUE)
        for (w in warn) warning(w, call. = FALSE)
      }
    }

    invisible(object)
  }
)

print_out <- function(label, maybe = FALSE) {
  status <- ifelse(maybe, "may extent out", "is out")
  sprintf("Date %s %s of calibration range.", dQuote(label), status)
}

