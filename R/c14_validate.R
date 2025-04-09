# 14C CALIBRATION CHECK
#' @include AllGenerics.R
NULL

#' Check Calibrated Radiocarbon Dates
#'
#' @param object A [`CalibratedAges-class`] object.
#' @param verbose A [`logical`] scalar: should extra information be reported?
#' @return
#'  `c14_validate()` is called it for its side-effects: it prints
#'  [warning messages][warnings()] if calibrated ages are (partially) out of
#'  calibration range. Invisibly returns `x`.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @keywords internal
c14_validate <- function(object, verbose = getOption("ananke.verbose")) {
  status <- object@status
  lab <- labels(object)

  if (isTRUE(verbose)) {
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

print_out <- function(label, maybe = FALSE) {
  status <- ifelse(maybe, "may extent out", "is out")
  sprintf("Date %s %s of calibration range.", dQuote(label), status)
}
