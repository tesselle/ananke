# COERCION
#' @include AllGenerics.R
NULL

# To list ======================================================================
#' @export
#' @method as.list CalibratedIntervals
as.list.CalibratedIntervals <- function(x, ..., calendar = get_calendar()) {
  z <- as.data.frame(x, calendar = calendar)
  f <- factor(z$label, levels = unique(z$label))
  z$label <- NULL
  split(x = z, f = f)
}

#' @export
#' @rdname as.list
#' @aliases as.list,CalibratedIntervals-method
setMethod("as.list", "CalibratedIntervals", as.list.CalibratedIntervals)

# To data.frame ================================================================
#' @export
#' @method as.data.frame CalibratedAges
as.data.frame.CalibratedAges <- function(x, ...,
                                         calendar = get_calendar()) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,CalibratedAges-method
setMethod("as.data.frame", "CalibratedAges", as.data.frame.CalibratedAges)

#' @export
#' @method as.data.frame CalibratedIntervals
as.data.frame.CalibratedIntervals <- function(x, ...,
                                        calendar = get_calendar()) {
  ## Build a data frame
  data.frame(
    label = labels(x),
    start = start(x, calendar = calendar),
    end = end(x, calendar = calendar),
    p = x@p
  )
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,CalibratedIntervals-method
setMethod("as.data.frame", "CalibratedIntervals", as.data.frame.CalibratedIntervals)

#' @export
#' @method as.data.frame RECE
as.data.frame.RECE <- function(x, ..., calendar = get_calendar()) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,RECE-method
setMethod("as.data.frame", "RECE", as.data.frame.RECE)

#' @export
#' @method as.data.frame ProxyRecord
as.data.frame.ProxyRecord <- function(x, ...,
                                         calendar = get_calendar()) {
  dens <- x[, , 1, drop = TRUE]
  z <- data.frame(aion::time(x, calendar = calendar), dens)
  colnames(z) <- c("time", colnames(x) %||% paste0("X", seq_len(NCOL(x))))
  z
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,ProxyRecord-method
setMethod("as.data.frame", "ProxyRecord", as.data.frame.ProxyRecord)
