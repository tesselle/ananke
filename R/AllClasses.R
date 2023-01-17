# CLASSES DEFINITION AND INITIALIZATION
NULL

#' Calibrated Radiocarbon Ages
#'
#' An S4 class to represent calibrated radiocarbon ages.
#' @param labels A [`character`] vector specifying the names of the ages (e.g.
#'  laboratory codes).
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curves
#'  used.
#' @slot start A length-one [`numeric`] vector specifying the beginning of the
#'  calibrated time range.
#' @slot resolution A length-one [`numeric`] vector specifying the resolution of
#'  the calibrated time range.
#' @slot F14C A [`logical`] scalar:
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedAges-class
#' @keywords internal
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    labels = "character",
    ages = "numeric",
    errors = "numeric",
    curves = "character",
    start = "numeric",
    resolution = "numeric",
    F14C = "logical",
    calendar = "character"
  ),
  contains = "matrix"
)
