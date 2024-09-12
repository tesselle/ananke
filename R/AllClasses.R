# CLASSES DEFINITION AND INITIALIZATION
NULL

# Import classes ===============================================================
#' @importClassesFrom aion TimeSeries
#' @importClassesFrom aion RataDie
NULL

# 14C calibration ==============================================================
#' Calibrated Radiocarbon Ages
#'
#' An S4 class to represent calibrated radiocarbon ages.
#' @param values A [`numeric`] vector giving the BP ages or F14C values to be
#'  calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the
#'  values to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curves
#'  used.
#' @param reservoir_offsets A [`numeric`] vector giving the offset values for
#'  any marine reservoir effect.
#' @param reservoir_errors A [`numeric`] vector giving the offset value errors
#'  for any marine reservoir effect.
#' @slot F14C A [`logical`] scalar: is `values` F14C instead of radiocarbon
#'  ages?
#' @slot status An [`integer`] vector specifying the calibration status.
#'  It must be one of "`0`" (OK), "`1`" (out of calibration range) or "`2`"
#'  (may extend out of calibration range).
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedAges-class
#' @keywords internal
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    values = "numeric",
    errors = "numeric",
    curves = "character",
    reservoir_offsets = "numeric",
    reservoir_errors = "numeric",
    F14C = "logical",
    status = "integer"
  ),
  contains = "TimeSeries"
)

#' Calibrated SPD
#'
#' An S4 class to represent summed probability distributions (SPD) of
#' calibrated radiocarbon ages.
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedSPD-class
#' @keywords internal
.CalibratedSPD <- setClass(
  Class = "CalibratedSPD",
  contains = "TimeSeries"
)

#' Calibrated Intervals
#'
#' An S4 class to represent HDR intervals of calibrated radiocarbon ages.
#' @slot p A [`numeric`] vector giving the probabilities.
#' @note
#'  This class inherits from [`aion::TimeIntervals-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedHDR-class
#' @keywords internal
.CalibratedHDR <- setClass(
  Class = "CalibratedHDR",
  slots = c(
    p = "numeric"
  ),
  contains = "TimeIntervals"
)

#' Radiocarbon Event Count Ensemble
#'
#' An S4 class to represent a radiocarbon event count ensemble.
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
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
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
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
