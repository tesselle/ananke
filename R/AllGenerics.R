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

#' @rdname mutators
#' @aliases get_calendar-method
setGeneric(
  name = "get_calendar",
  def = function(x) standardGeneric("get_calendar")
)

# Time Scale ===================================================================
## BP --------------------------------------------------------------------------
#' Before Present
#'
#' Converts between BP (Before Present) and CE (Common Era) or b2k (before 2000)
#' time scales.
#' @param object An object.
#' @return
#'  * `BP_to_CE()` and `BP_to_b2k()` return an object of the same sort as
#'    `object` with a new time scale.
#'  * `is_BP()` returns a [`logical`] scalar.
#' @note
#'  There is no year \eqn{0} in BCE/CE scale.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @family time scales
#' @docType methods
#' @name BP
#' @rdname BP
NULL

#' @rdname BP
#' @aliases BP_to_CE-method
setGeneric(
  name = "BP_to_CE",
  def = function(object) standardGeneric("BP_to_CE")
)

#' @rdname BP
#' @aliases BP_to_b2k-method
setGeneric(
  name = "BP_to_b2k",
  def = function(object) standardGeneric("BP_to_b2k")
)

#' @rdname BP
#' @aliases is_BP-method
setGeneric(
  name = "is_BP",
  def = function(object) standardGeneric("is_BP")
)

## CE --------------------------------------------------------------------------
#' Common Era
#'
#' Converts between CE (Common Era) and BP (Before Present) or b2k (before 2000)
#' time scales.
#' @param object An object.
#' @return
#'  * `CE_to_BP()` and `CE_to_b2k()` return an object of the same sort as
#'    `object` with a new time scale.
#'  * `is_CE()` returns a [`logical`] scalar.
#' @note
#'  There is no year \eqn{0} in BCE/CE scale.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @family time scales
#' @docType methods
#' @name CE
#' @rdname CE
NULL

#' @rdname CE
#' @aliases CE_to_BP-method
setGeneric(
  name = "CE_to_BP",
  def = function(object) standardGeneric("CE_to_BP")
)

#' @rdname CE
#' @aliases CE_to_b2k-method
setGeneric(
  name = "CE_to_b2k",
  def = function(object) standardGeneric("CE_to_b2k")
)

#' @rdname CE
#' @aliases is_CE-method
setGeneric(
  name = "is_CE",
  def = function(object) standardGeneric("is_CE")
)

## b2k -------------------------------------------------------------------------
#' Before 2000
#'
#' Converts b2k (before 2000) and between BP (Before Present) or CE (Common Era)
#' time scales.
#' @param object An object.
#' @return
#'  * `b2k_to_CE()` and `b2k_to_BP()` return an object of the same sort as
#'    `object` with a new time scale.
#'  * `is_b2k()` returns a [`logical`] scalar.
#' @note
#'  There is no year \eqn{0} in BCE/CE scale.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @family time scales
#' @docType methods
#' @name b2k
#' @rdname b2k
NULL

#' @rdname b2k
#' @aliases b2k_to_BP-method
setGeneric(
  name = "b2k_to_BP",
  def = function(object) standardGeneric("b2k_to_BP")
)

#' @rdname b2k
#' @aliases b2k_to_CE-method
setGeneric(
  name = "b2k_to_CE",
  def = function(object) standardGeneric("b2k_to_CE")
)

#' @rdname b2k
#' @aliases is_b2k-method
setGeneric(
  name = "is_b2k",
  def = function(object) standardGeneric("is_b2k")
)

# Radiocarbon ==================================================================
#' 14C Calibration Curve
#'
#' @param x A [`character`] string naming a calibration curve
#' @param ... Currently not used.
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
#' @param drop A [`logical`] scalar: should years with zero probability be
#'  discarded? If `TRUE` (the default), results in a narrower time range.
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
#' @param ... Currently not used.
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

#' Plot Calibrated Radiocarbon Ages
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function.
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name plot_14C
#' @rdname plot_14C
NULL

# Allen Interval Algebra =======================================================
#' Allen Relation Between Definite Intervals
#'
#' @param x,y A [`numeric`] vector giving the lower and upper boundaries of the
#'  time intervals, respectively. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @details
#'
#' \tabular{lrlr}{
#'  **Relation** \tab     \tab     \tab  **Converse** \cr
#'  precedes     \tab (p) \tab (P) \tab   preceded by \cr
#'  meets        \tab (m) \tab (M) \tab        met by \cr
#'  overlaps     \tab (o) \tab (O) \tab overlapped by \cr
#'  finished by  \tab (F) \tab (f) \tab      finishes \cr
#'  contains     \tab (D) \tab (d) \tab        during \cr
#'  starts       \tab (s) \tab (S) \tab    started by \cr
#'  equals       \tab (e) \tab     \tab               \cr
#' }
#'
#' @return
#'  A [`character`] matrix specifying the Allen relations.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#'
#'  Alspaugh, T. (2019). Allen's Interval Algebra.
#'  URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_relation-method
setGeneric(
  name = "allen_relation",
  def = function(x, y, ...) standardGeneric("allen_relation")
)

#' Complement of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_complement-method
setGeneric(
  name = "allen_complement",
  def = function(x, ...) standardGeneric("allen_complement")
)

#' Converse of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_converse-method
setGeneric(
  name = "allen_converse",
  def = function(x, ...) standardGeneric("allen_converse")
)

#' Composition of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_composition-method
setGeneric(
  name = "allen_composition",
  def = function(x, y, ...) standardGeneric("allen_composition")
)

#' Intersection of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_intersect-method
setGeneric(
  name = "allen_intersect",
  def = function(x, y, ...) standardGeneric("allen_intersect")
)

#' Union of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_union-method
setGeneric(
  name = "allen_union",
  def = function(x, y, ...) standardGeneric("allen_union")
)
