# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HDR ==========================================================================
#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,CalibratedAges,missing-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "CalibratedAges", y = "missing"),
  definition = function(x, level = 0.954,
                        calendar = getOption("ananke.calendar"), ...) {
    hdr <- apply(
      X = x,
      MARGIN = 2,
      FUN = function(y, x, level, ...) {
        arkhe::interval_hdr(x, as.numeric(y), level, ...)
      },
      x = aion::time(x, calendar = NULL),
      level = level,
      simplify = FALSE
    )
    names(hdr) <- colnames(x)
    if (is.null(calendar)) return(hdr)
    lapply(
      X = hdr,
      FUN = function(x, calendar) {
        x[, 1] <- aion::as_year(x[, 1], calendar = calendar)
        x[, 2] <- aion::as_year(x[, 2], calendar = calendar)
        x
      },
      calendar = calendar
    )
  }
)
