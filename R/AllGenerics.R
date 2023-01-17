# GENERIC METHODS
#' @include AllClasses.R
#' @importFrom methods new setGeneric setMethod .valueClassTest
NULL

# Tools ========================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

# Calibration ==================================================================
#' 14C Calibration Curve
#'
#' @param x A [`character`] string naming a calibration curve
#' @return
#'  A three-column [`data.frame`]: calibrated age BP, uncalibrated age BP and
#'  standard deviation.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases get_curve-method
setGeneric(
  name = "get_curve",
  def = function(x, ...) standardGeneric("get_curve"),
  valueClass = "data.frame"
)

#' 14C Calibration
#'
#' Calibrates radiocarbon dates.
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param names A [`character`] vector specifying the names of the ages (e.g.
#'  laboratory codes).
#' @param curves A [`character`] vector specifying the calibration curve to be
#'  used. Different curves can be specified per dated sample.
#' @param reservoir_offsets A [`numeric`] vector giving the offset values for
#'  any marine reservoir effect (defaults to 0; i.e. no offset).
#' @param reservoir_errors A [`numeric`] vector giving the offset value errors
#'  for any marine reservoir effect (defaults to 0; i.e. no offset).
#' @param from length-one [`numeric`] vector specifying the earliest data to
#'  calibrate for, in cal. BP years.
#' @param to A length-one [`numeric`] vector specifying the latest data to
#'  calibrate for, in cal. BP years.
#' @param resolution A length-one [`numeric`] vector specifying the temporal
#'  resolution (in years) of the calibration.
#' @param normalize A [`logical`] scalar: should the calibration be normalized?
#' @param F14C A [`logical`] scalar: should the calibration be carried out in
#'  F14C space?
#' @param eps A length-one [`numeric`] value giving the cutoff below which
#'  calibration values will be removed.
#' @param ... Currently not used.
#' @return
#'  A [`CalibratedAges-class`] object.
#' @references
#'  Bronk Ramsey, C. (2008). RADIOCARBON DATING: REVOLUTIONS IN UNDERSTANDING.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#' @note
#'  Adapted from [rcarbon::calibrate()].
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases calibrate-method
setGeneric(
  name = "calibrate",
  def = function(ages, errors, ...) standardGeneric("calibrate"),
  valueClass = "CalibratedAges"
)

#' F14C
#'
#' Converts F14C ratio to 14C age.
#' @param ages A [`numeric`] vector giving the radiocarbon ages.
#' @param ratios A [`numeric`] vector giving the F14C ratios.
#' @param errors A [`numeric`] vector giving the standard deviation of the
#'  ages/ratios.
#' @return
#'  A two-column [`data.frame`].
#' @references
#'  Bronk Ramsey, C. (2008). RADIOCARBON DATING: REVOLUTIONS IN UNDERSTANDING.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name F14C
#' @rdname F14C
NULL

#' @rdname F14C
#' @aliases BP14C_to_F14C-method
setGeneric(
  name = "BP14C_to_F14C",
  def = function(ages, errors, ...) standardGeneric("BP14C_to_F14C"),
  valueClass = "data.frame"
)

#' @rdname F14C
#' @aliases F14C_to_BP14C-method
setGeneric(
  name = "F14C_to_BP14C",
  def = function(ratios, errors, ...) standardGeneric("F14C_to_BP14C"),
  valueClass = "data.frame"
)
