# CLASSES DEFINITION AND INITIALIZATION
NULL

# 14C calibration ==============================================================
#' Calibrated Radiocarbon Ages
#'
#' An S4 class to represent calibrated radiocarbon ages.
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curves
#'  used.
#' @slot F14C A [`logical`] scalar:
#' @slot status An [`integer`] vector specifying the calibration status.
#'  It must be one of "`0`" (OK), "`1`" (out of calibration range) or "`2`"
#'  (may extend out of calibration range).
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedAges-class
#' @keywords internal
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    ages = "numeric",
    errors = "numeric",
    curves = "character",
    F14C = "logical",
    status = "integer"
  ),
  contains = "TimeSeries"
)

#' Calibrated SPD
#'
#' An S4 class to represent summed probability distributions (SPD) of
#' radiocarbon dates.
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedSPD-class
#' @keywords internal
.CalibratedSPD <- setClass(
  Class = "CalibratedSPD",
  contains = "TimeSeries"
)

#' Radiocarbon Event Count Ensemble
#'
#' An S4 class to represent a radiocarbon event count ensemble.
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RECE-class
#' @keywords internal
.RECE <- setClass(
  Class = "RECE",
  contains = "TimeSeries"
)

# Proxy Record =================================================================
#' Proxy Record
#'
#' An S4 class to store proxy records.
#' @slot year A [`numeric`] vector giving the time points at which the
#'  distribution is estimated.
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases ProxyRecord-class
#' @keywords internal
.ProxyRecord <- setClass(
  Class = "ProxyRecord",
  slots = c(
    density = "matrix",
    proxy = "numeric"
  ),
  contains = "TimeSeries"
)
