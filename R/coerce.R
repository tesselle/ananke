# COERCION
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame CalibratedAges
as.data.frame.CalibratedAges <- function(x, ...,
                                         calendar = getOption("ananke.calendar")) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,CalibratedAges-method
setMethod("as.data.frame", "CalibratedAges", as.data.frame.CalibratedAges)

#' @export
#' @method as.data.frame RECE
as.data.frame.RECE <- function(x, ..., calendar = getOption("ananke.calendar")) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,RECE-method
setMethod("as.data.frame", "RECE", as.data.frame.RECE)

#' @export
#' @method as.data.frame ProxyRecord
as.data.frame.ProxyRecord <- function(x, ...,
                                         calendar = getOption("ananke.calendar")) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,ProxyRecord-method
setMethod("as.data.frame", "ProxyRecord", as.data.frame.ProxyRecord)
