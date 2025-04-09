# GENERIC METHODS
#' @include AllClasses.R
NULL

# Import S4 generics ===========================================================
#' @importMethodsFrom arkhe describe
#' @importMethodsFrom arkhe interval_credible
#' @importMethodsFrom arkhe interval_hdr
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

#' Find Labels from Object
#'
#' Find a suitable set of labels from an object for use in printing or plotting,
#' for example.
#' @param object An object from which to find labels.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name labels
#' @rdname labels
NULL

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j,k Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
# @param ... Currently not used.
#' @return
#'  A subsetted object.
# @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

## Coerce ----------------------------------------------------------------------
#' Coerce to a Data Frame
#'
#' @inheritParams as.list
#' @return
#'  A [`data.frame`] with an extra `time` column.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name as.data.frame
#' @rdname as.data.frame
NULL

#' Coerce to a list
#'
#' @param x An object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param ... Currently not used.
#' @return
#'  A [`list`].
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name as.list
#' @rdname as.list
NULL

# Radiocarbon ==================================================================
## Calibration curve -----------------------------------------------------------
#' 14C Calibration Curve
#'
#' @param name A [`character`] vector naming calibration curves (see details).
#' @param ... Currently not used.
#' @details
#'  The following calibration curves are available:
#'
#' \tabular{ll}{
#'  **Curve**    \tab **Reference** \cr
#'  `bomb04nh1`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04nh2`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04nh3`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04sh`   \tab Hua and Barbetti 2004 \cr
#'  `bomb13nh1`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13nh2`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13nh3`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13sh12` \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13sh3`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb21nh1`  \tab Hua et al. 2022 \cr
#'  `bomb21nh2`  \tab Hua et al. 2022 \cr
#'  `bomb21nh3`  \tab Hua et al. 2022 \cr
#'  `bomb21sh12` \tab Hua et al. 2022 \cr
#'  `bomb21sh3`  \tab Hua et al. 2022 \cr
#'  `cariaco04`  \tab Hughen et al. 2004 \cr
#'  `intcal98`   \tab Stuiver et al. 1998 \cr
#'  `intcal04`   \tab Reimer et al. 2004 \cr
#'  `intcal09`   \tab Reimer et al. 2009 \cr
#'  `intcal13`   \tab Reimer et al. 2013 \cr
#'  `intcal20`   \tab Reimer et al. 2020 \cr
#'  `kueppers04` \tab Kueppers et al. 2004 \cr
#'  `marine98`   \tab Stuiver, Reimer and Braziunas 1998 \cr
#'  `marine04`   \tab Hughen et al. 2004 \cr
#'  `marine09`   \tab Reimer et al. 2009 \cr
#'  `marine13`   \tab Reimer et al. 2013 \cr
#'  `marine20`   \tab Heaton et al. 2020 \cr
#'  `shcal04`    \tab McCormac et al. 2004 \cr
#'  `shcal13`    \tab Hogg et al. 2013 \cr
#'  `shcal20`    \tab Hogg et al. 2020 \cr
#' }
#'
#' @return
#'  A `list` of three-column [`data.frame`]:
#'  \tabular{ll}{
#'  `CALBP` \tab Calibrated age BP            \cr
#'  `AGE`   \tab Uncalibrated radiocarbon age \cr
#'  `ERROR` \tab Standard deviation           \cr
#'  }
#' @references
#'  Heaton, Timothy J, Peter Köhler, Martin Butzin, Edouard Bard, Ron W Reimer,
#'  William E N Austin, Christopher Bronk Ramsey, et al. (2020). Marine20 The
#'  Marine Radiocarbon Age Calibration Curve (0-55,000 Cal BP).
#'  *Radiocarbon*, 62(4): 779-820. \doi{10.1017/RDC.2020.68}.
#'
#'  Hogg, Alan G, Timothy J Heaton, Quan Hua, Jonathan G Palmer, Chris SM
#'  Turney, John Southon, Alex Bayliss, et al. (2020). SHCal20 Southern
#'  Hemisphere Calibration, 0-55,000 Years Cal BP. *Radiocarbon*, 62(4): 759-78.
#'  \doi{10.1017/RDC.2020.59}.
#'
#'  Hogg, Alan G, Quan Hua, Paul G Blackwell, Mu Niu, Caitlin E Buck, Thomas P
#'  Guilderson, Timothy J Heaton, et al. (2013). SHCal13 Southern Hemisphere
#'  Calibration, 0-50,000 Years Cal BP. *Radiocarbon*, 55(4): 1889-1903.
#'  \doi{10.2458/azu_js_rc.55.16783}.
#'
#'  Hua, Quan, and Mike Barbetti (2004). Review of Tropospheric Bomb 14C Data
#'  for Carbon Cycle Modeling and Age Calibration Purposes. *Radiocarbon*,
#'  46(3): 1273-1298. \doi{10.1017/S0033822200033142}.
#'
#'  Hua, Quan, Mike Barbetti, and Andrzej Z Rakowski (2013). Atmospheric
#'  Radiocarbon for the Period 1950-2010. *Radiocarbon*, 55(4): 2059‑2072.
#'  \doi{10.2458/azu_js_rc.v55i2.16177}.
#'
#'  Hua, Quan, Jocelyn C Turnbull, Guaciara M Santos, Andrzej Z Rakowski,
#'  Santiago Ancapichún, Ricardo De Pol-Holz, Samuel Hammer, et al. (2022).
#'  Atmospheric Radiocarbon for the Period 1950-2019. *Radiocarbon*,
#'  64(4): 723‑745. \doi{10.1017/RDC.2021.95}.
#'
#'  Hughen, K., S. Lehman, J. Southon, J. Overpeck, O. Marchal, C. Herring,
#'  and J. Turnbull (2004). 14C Activity and Global Carbon Cycle Changes over
#'  the Past 50,000 Years. *Science*, 303(5655): 202‑207.
#'  \doi{10.1126/science.1090300}.
#'
#'  Hughen, Konrad A, Mike G L Baillie, Edouard Bard, J Warren Beck, Chanda J H
#'  Bertrand, Paul G Blackwell, Caitlin E Buck, et al. (2004). Marine04 Marine
#'  Radiocarbon Age Calibration, 0-26 cal kyr BP. *Radiocarbon*,
#'  46(3): 1059‑1086. \doi{10.1017/S0033822200033002}.
#'
#'  Kueppers, Lara M., John Southon, Paul Baer, and John Harte (2004). Dead Wood
#'  Biomass and Turnover Time, Measured by Radiocarbon, along a Subalpine
#'  Elevation Gradient. *Oecologia*, 141(4): 641‑651.
#'  \doi{10.1007/s00442-004-1689-x}.
#'
#'  McCormac, F G, A G Hogg, P G Blackwell, C E Buck, T F G Higham, and P J
#'  Reimer (2004). Shcal04 Southern Hemisphere Calibration, 0-11.0 cal kyr BP.
#'  *Radiocarbon*, 46(3): 1087‑1092. \doi{10.1017/S0033822200033014}.
#'
#'  Reimer, P J, M G L Baillie, E Bard, A Bayliss, J W Beck, P G Blackwell,
#'  C Bronk Ramsey, et al. (2009). IntCal09 and Marine09 Radiocarbon Age
#'  Calibration Curves, 0-50,000 Years cal BP. *Radiocarbon*, 51(4): 1111‑1150.
#'  \doi{10.1017/S0033822200034202}.
#'
#'  Reimer, Paula J, William E N Austin, Edouard Bard, Alex Bayliss, Paul G
#'  Blackwell, Christopher Bronk Ramsey, Martin Butzin, et al. (2020).
#'  The IntCal20 Northern Hemisphere Radiocarbon Age Calibration Curve
#'  (0-55 cal kBP). *Radiocarbon*, 62(4): 725‑757. \doi{10.1017/RDC.2020.41}.
#'
#'  Reimer, Paula J, Mike G L Baillie, Edouard Bard, Alex Bayliss,
#'  J Warren Beck, Chanda J H Bertrand, Paul G Blackwell, et al. (2004).
#'  Intcal04 Terrestrial Radiocarbon Age Calibration, 0-26 cal kyr BP.
#'  *Radiocarbon*, 46(3): 1029‑1058. \doi{10.1017/S0033822200032999}.
#'
#'  Reimer, Paula J, Edouard Bard, Alex Bayliss, J Warren Beck, Paul G
#'  Blackwell, Christopher Bronk Ramsey, Caitlin E Buck, et al. (2013).
#'  IntCal13 and Marine13 Radiocarbon Age Calibration Curves 0-50,000
#'  Years cal BP. *Radiocarbon*, 55(4): 1869‑1887.
#'  \doi{10.2458/azu_js_rc.55.16947}.
#'
#'  Stuiver, Minze, Paula J. Reimer, Edouard Bard, J. Warren Beck, G. S. Burr,
#'  Konrad A. Hughen, Bernd Kromer, Gerry McCormac, Johannes van der Plicht, and
#'  Marco Spurk (1998). INTCAL98 Radiocarbon Age Calibration, 24,000-0 cal BP.
#'  *Radiocarbon*, 40(3): 1041‑1083. \doi{10.1017/S0033822200019123}.
#'
#'  Stuiver, Minze, Paula J. Reimer, and Thomas F. Braziunas. (1998).
#'  High-Precision Radiocarbon Age Calibration for Terrestrial and Marine
#'  Samples. *Radiocarbon*, 40(3): 1127‑1151. \doi{10.1017/S0033822200019172}.
#' @example inst/examples/ex-14c-curve.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_curve-method
setGeneric(
  name = "c14_curve",
  def = function(name, ...) standardGeneric("c14_curve")
)

## Calibration -----------------------------------------------------------------
#' 14C Calibration
#'
#' Calibrates radiocarbon ages.
#' @param values A [`numeric`] vector giving the BP ages or F14C values to be
#'  calibrated (conventional ages).
#' @param errors A [`numeric`] vector giving the errors associated to the
#'  values to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curve to be
#'  used. Different curves can be specified per sample.
#' @param names A [`character`] vector specifying the names of the samples (e.g.
#'  laboratory codes).
#' @param positions A [`numeric`] vector giving the position values (e.g.
#'  depths) for each age.
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
#'  F14C space? If `TRUE`, `values` must be expressed as F14C.
#' @param drop A [`logical`] scalar: should years with zero probability be
#'  discarded? If `TRUE` (the default), results in a narrower time range.
#' @param eps A length-one [`numeric`] value giving the cutoff below which
#'  calibration values will be removed.
#' @param dfs A [`character`] vector giving the degrees-of-freedom values for
#'  the t-distribution associated with the calibration calculation. A large
#'  value indicates Gaussian distributions assumed for the 14C ages.
#' @param verbose A [`logical`] scalar: should extra information be reported
#'  (e.g. warning message for dates out of calibration range)?
#' @param ... Currently not used.
#' @return
#'  A [`CalibratedAges-class`] object.
#' @references
#'  Bronk Ramsey, C. (2008). Radiocarbon Dating: Revolutions in Understanding.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#' @note
#'  Adapted from [Bchron::BchronCalibrate()] by Andrew Parnell and
#'  [rcarbon::calibrate()] by Andrew Bevan and Enrico Crema.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_calibrate-method
setGeneric(
  name = "c14_calibrate",
  def = function(values, errors, ...) standardGeneric("c14_calibrate"),
  valueClass = "CalibratedAges"
)

#' Check Calibrated Radiocarbon Dates
#'
#' @param object A [`CalibratedAges-class`] object.
#' @param ... Currently not used.
#' @return
#'  `c14_validate()` is called it for its side-effects: it prints
#'  [warning messages][warnings()] if calibrated ages are (partially) out of
#'  calibration range. Invisibly returns `x`.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_validate-method
setGeneric(
  name = "c14_validate",
  def = function(object, ...) standardGeneric("c14_validate"),
  valueClass = "CalibratedAges"
)

#' Uncalibrate a Radiocarbon Date
#'
#' @param object A [`CalibratedAges-class`] object or a [`numeric`] vector of
#'  calibrated ages (BP).
#' @param n An [`integer`] specifying the number of random samples.
#' @param curves A [`character`] vector specifying the calibration curve to be
#'  used. Different curves can be specified.
#' @param ... Currently not used.
#' @example inst/examples/ex-14c-uncalibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_uncalibrate-method
setGeneric(
  name = "c14_uncalibrate",
  def = function(object, ...) standardGeneric("c14_uncalibrate")
)

## F14C ------------------------------------------------------------------------
#' F14C
#'
#' Converts F14C values to 14C ages.
#' @param values A [`numeric`] vector giving the radiocarbon ages or the F14C
#'  values.
#' @param errors A [`numeric`] vector giving the standard deviations.
#' @param lambda A length-one [`numeric`] vector specifying the mean-life of
#'  radiocarbon (defaults to 14C half-life value as introduced by Libby 1952).
#' @param asymmetric A [`logical`] scalar: should asymmetric 14C errors be
#'  returned (van der Plicht & Hogg, 2006)?
#' @param rounding A [`character`] string specifying the rounding convention.
#'  It can be one of "`none`" (the default, no rounding) or "`stuiver`"
#'  (Stuiver & Polach, 1977). Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @return
#'  A [`data.frame`].
#' @references
#'  Bronk Ramsey, C. (2008). Radiocarbon Dating: Revolutions in Understanding.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#'
#'  Stuiver, M., Polach, H. A. (1977). Discussion Reporting of 14C Data.
#'  *Radiocarbon*, 19(3): 355-363. \doi{10.1017/S0033822200003672}.
#'
#'  van der Plicht, J., Hogg, A. (2006). A Note on Reporting Radiocarbon.
#'  *Quaternary Geochronology*, 1(4): 237-240.
#'  \doi{10.1016/j.quageo.2006.07.001}.
#' @example inst/examples/ex-f14c.R
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
  def = function(values, errors, ...) standardGeneric("BP14C_to_F14C"),
  valueClass = "data.frame"
)

#' @rdname F14C
#' @aliases F14C_to_BP14C-method
setGeneric(
  name = "F14C_to_BP14C",
  def = function(values, errors, ...) standardGeneric("F14C_to_BP14C"),
  valueClass = "data.frame"
)

## Combine ---------------------------------------------------------------------
#' Combine 14C
#'
#' Combines radiocarbon dates.
#' @param values A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param groups A [`factor`] in the sense that `as.factor(groups)` defines the
#'  the groups to combine with. If `NULL` (the default), all dates are combined.
#'  `NA`s will be treated as isolated dates.
#' @param ... Currently not used.
#' @return
#'  A [`data.frame`] with the following columns:
#'  \tabular{ll}{
#'  `groups` \tab Group names                      \cr
#'  `ages`   \tab Combined 14C ages                \cr
#'  `errors` \tab Combined 14C standard deviations \cr
#'  `chi2`   \tab Chi-squared test statistic       \cr
#'  `p`      \tab Chi-squared test p-value         \cr
#'  }
#' @references
#'  Ward, G. K. and Wilson, S. R. (1978). Procedures for Comparing and Combining
#'  Radiocarbon Age Determinations: A Critique. *Archaeometry* 20(1): 19‑31.
#'  \doi{10.1111/j.1475-4754.1978.tb00208.x}.
#' @example inst/examples/ex-14c-combine.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_combine-method
setGeneric(
  name = "c14_combine",
  def = function(values, errors, ...) standardGeneric("c14_combine"),
  valueClass = "data.frame"
)

## Plot ------------------------------------------------------------------------
#' Plot Calibrated Radiocarbon Ages
#'
#' @param x A [`CalibratedAges-class`] or [`CalibratedSPD-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param density A [`logical`] scalar: should density be drawn?
#' @param interval A [`character`] string specifying the intervals to be drawn.
#'  It must be one of "`hrd`" (the default), "`credible`" or "`none`".
#'  Any unambiguous substring can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Only used if `interval` is `TRUE`.
#' @param fixed A [`logical`] scalar: should a fixed y scale be used?
#'  If `TRUE` (the default), ages are equally spaced along the y axis.
#'  If `FALSE`, age `positions` are used (see [c14_calibrate()]).
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param col.density,col.interval A specification for the plotting colors.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function.
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-14c-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name c14_plot
#' @rdname c14_plot
NULL

#' Plot a Radiocarbon Event Count Ensemble
#'
#' @param x An [`RECE-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param ... Further parameters to be passed to [graphics::image()].
#' @return
#'  `image()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @references
#'  Carleton, W. C. (2021). Evaluating Bayesian Radiocarbon‐dated Event Count
#'  (REC) Models for the Study of Long‐term Human and Environmental Processes.
#'  *Journal of Quaternary Science*, 36(1): 110‑23. \doi{10.1002/jqs.3256}.
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name rec_plot
#' @rdname rec_plot
NULL

## Sample ----------------------------------------------------------------------
#' Sample Calibrated Ages
#'
#' @param object A [`CalibratedAges-class`] object.
#' @param n An [`integer`] specifying the number of random samples.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). Defaults to [aion::CE()]. If `NULL`,
#'  *rata die* are returned.
#' @param ... Currently not used.
#' @example inst/examples/ex-14c-sample.R
#' @return An `numeric` matrix.
#' @author N. Frerebeau
#' @family radiocarbon tools
#' @docType methods
#' @aliases c14_sample-method
setGeneric(
  name = "c14_sample",
  def = function(object, ...) standardGeneric("c14_sample")
)

## RECE ------------------------------------------------------------------------
#' Radiocarbon Event Count
#'
#' @param object A [`CalibratedAges-class`] object.
#' @param from length-one [`numeric`] vector specifying the earliest data to
#'  calibrate for (in cal BP years).
#' @param to A length-one [`numeric`] vector specifying the latest data to
#'  calibrate for (in cal BP years).
#' @param by A length-one [`numeric`] vector specifying the temporal
#'  resolution (in years) of the calibration.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). Defaults to [aion::CE()]. If `NULL`,
#'  *rata die* are returned.
#' @param n An [`integer`] specifying the number of item to choose randomly.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @return
#'  An [`RECE-class`] object.
#' @references
#'  Carleton, W. C. (2021). Evaluating Bayesian Radiocarbon‐dated Event Count
#'  (REC) Models for the Study of Long‐term Human and Environmental Processes.
#'  *Journal of Quaternary Science*, 36(1): 110‑23. \doi{10.1002/jqs.3256}.
#' @author N. Frerebeau
#' @family radiocarbon tools
#' @docType methods
#' @aliases c14_ensemble-method
setGeneric(
  name = "c14_ensemble",
  def = function(object, ...) standardGeneric("c14_ensemble"),
  valueClass = "RECE"
)

## SPD -------------------------------------------------------------------------
#' Summed Probability Distributions
#'
#' Computes summed probability distributions (SPD) of radiocarbon dates.
#' @param object A [`CalibratedAges-class`] object.
#' @param normalize_date A [`logical`] scalar: should the total probability mass
#'  of the calibrated dates be normalised (to sum to unity within the time-span
#'  of analysis)?
#' @param normalize_spd A [`logical`] scalar: should the total probability mass
#'  of the SPD be normalised (to sum to unity)?
#' @param ... Currently not used.
#' @details
#'  Summed probability distributions (SPD) are not statistically valid
#'  estimators of the calendar age of a potential future sample. They should not
#'  be used in any dates-as-data approach to provide a population proxy.
#' @return
#'  A [`CalibratedSPD-class`] object.
#' @example inst/examples/ex-14c-spd.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_spd-method
setGeneric(
  name = "c14_spd",
  def = function(object, ...) standardGeneric("c14_spd"),
  valueClass = "CalibratedSPD"
)

# Interval =====================================================================
## HDR -------------------------------------------------------------------------
#' Highest Density Regions
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @return
#'  A [`CalibratedIntervals-class`] object.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-14c-hdr.R
#' @seealso [stats::density()], [arkhe::interval_hdr()]
#' @author N. Frerebeau
#' @family statistics
#' @docType methods
#' @rdname interval_hdr
#' @name interval_hdr
NULL

## Credible --------------------------------------------------------------------
#' Bayesian Credible Interval
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param n An [`integer`] specifying the number of random samples.
#' @param ... Currently not used.
#' @return
#'  A [`CalibratedIntervals-class`] object.
#' @example inst/examples/ex-14c-credible.R
#' @seealso [arkhe::interval_credible()]
#' @author N. Frerebeau
#' @family statistics
#' @docType methods
#' @rdname interval_credible
#' @name interval_credible
NULL

# Proxy Records ================================================================
#' Layer-Counted Proxy Records Uncertainties
#'
#' Represents layer-counted proxy records as sequences of probability
#' distributions on absolute, error-free time axes.
#' @param positions A positive [`numeric`] vector giving the positions (e.g.
#'  depths) at which proxy values and calendar ages were measured. In the case
#'  of layers of non-zero thickness, this should be the middle value of the
#'  slice. It must be in decreasing order (i.e. in chronological order).
#' @param proxy_values A [`numeric`] vector giving the proxy values.
#' @param proxy_errors A [`numeric`] vector giving the proxy uncertainties.
#' @param time_values A [`numeric`] vector giving the calendar ages (in years).
#' @param time_errors A [`numeric`] vector giving the calendar age uncertainties
#'  (in years).
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `time` (see [aion::calendar()]).
#' @param step A length-one [`numeric`] vector specifying the step size
#'  (in units of `proxy`) at which proxy records densities are to be estimated.
#' @param from A length-one [`numeric`] vector specifying the starting value of
#'  the temporal sequence at which densities are to be estimated (in years).
#' @param to A length-one [`numeric`] vector specifying the end value of the
#'  temporal sequence at which densities are to be estimated (in cal BP years).
#' @param by A length-one [`numeric`] vector specifying the increment of
#'  the temporal sequence at which densities are to be estimated (in years).
#' @param n An [`integer`] specifying the number of item to choose randomly.
#' @param verbose A [`logical`] scalar: should extra information be reported?
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @return
#'  A [`ProxyRecord-class`] object.
#' @references
#'  Boers, N., Goswami, B. & Ghil, M. (2017). A Complete Representation of
#'  Uncertainties in Layer-Counted Paleoclimatic Archives. *Climate of the
#'  Past*, 13(9): 1169-1180. \doi{10.5194/cp-13-1169-2017}.
#' @example inst/examples/ex-proxy.R
#' @author N. Frerebeau
#' @family proxy tools
#' @docType methods
#' @aliases proxy_ensemble-method
setGeneric(
  name = "proxy_ensemble",
  def = function(positions, ...) standardGeneric("proxy_ensemble"),
  valueClass = "ProxyRecord"
)

#' Plot Layer-Counted Proxy Records Uncertainties
#'
#' @param x A [`ProxyRecord-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param iqr A [`logical`] scalar: should the mean and IQR be displayed?
#' @param xlab,ylab A [`character`] string giving a label for the x and y axis.
#' @param col A list of colors such as that generated by [grDevices::hcl.colors()].
#' @param col.mean,col.iqr A specification for the line colors. Only used if
#'  `iqr` is `TRUE`.
#' @param lty.mean,lty.iqr A specification for the line types. Only used if
#'  `iqr` is `TRUE`.
#' @param lwd.mean,lwd.iqr A specification for the line widths. Only used if
#'  `iqr` is `TRUE`.
#' @param ... Further parameters to be passed to [graphics::image()].
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-proxy.R
#' @author N. Frerebeau
#' @docType methods
#' @family proxy tools
#' @name proxy_plot
#' @rdname proxy_plot
NULL

# Isotopes =====================================================================
#' Geological Model Age from Lead Isotope Analysis
#'
#' Compute geological model age (T) and U/Pb (mu) and Th/U (kappa) ratios from
#' lead isotopic measurements.
#' @param x A [`numeric`] vector of 206Pb/204Pb ratios. If `y` and `z` are
#'  missing, must be a [`list`] (or a [`data.frame`]) with `numeric` components
#'  (columns) `x`, `y` and `z`.
#' @param y A [`numeric`] vector of 207Pb/204Pb ratios.  If missing, an attempt
#'  is made to interpret `x` in a suitable way.
#' @param z A [`numeric`] vector of 208Pb/204Pb ratios.  If missing, an attempt
#'  is made to interpret `x` in a suitable way.
#' @param t0 A [`numeric`] value giving the time of the second stage of the
#'  reference model.
#' @param x_star A [`numeric`] value giving the 206Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param y_star A [`numeric`] value giving the 207Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param z_star A [`numeric`] value giving the 208Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param mu A [`numeric`] value giving the 238U/204Pb ratio of the
#'  reference model.
#' @param kappa A [`numeric`] value giving the 232Th/238U ratio of the
#'  reference model.
#' @param th232 A [`numeric`] value giving the decay constants of 232Th.
#' @param u238 A [`numeric`] value giving the decay constants of 238U.
#' @param u235 A [`numeric`] value giving the decay constants of 235U.
#' @param u238_235 A [`numeric`] value giving the actual 238U/235U ratio.
#' @param tolerance A [`numeric`] value specifying the tolerance (stopping
#'  criteria for the Newton–Raphson method).
#' @param stop An [`integer`] giving the stopping rule (i.e. maximum number of
#'  iterations) to avoid infinite loop.
#' @param ... Currently not used.
#' @note
#'  Reference values from Albarede & Juteau (1984).
#' @return
#'  A four columns [`data.frame`]:
#'  \describe{
#'   \item{`age`}{Geological model age (in Ma).}
#'   \item{`mu`}{238U/204Pb ratio.}
#'   \item{`kappa`}{232Th/238U ratio.}
#'   \item{`residual`}{Newton loop residual.}
#'  }
#' @references
#'  Albarède, F., Desaulty, A.-M. & Blichert-Toft, J. (2012). A Geological
#'  Perspective on the Use of Pb Isotopes in Archaeometry. *Archaeometry*, 54:
#'  853-867. \doi{10.1111/j.1475-4754.2011.00653.x}.
#'
#'  Albarède, F. & Juteau, M. (1984). Unscrambling the Lead Model Ages.
#'  *Geochimica et Cosmochimica Acta*, 48(1): 207-12.
#'  \doi{10.1016/0016-7037(84)90364-8}.
#'
#'  Allègre, C. (2005). *Géologie isotopique*. Belin sup. Paris: Belin.
#' @example inst/examples/ex-lia.R
#' @author N. Frerebeau, F. Albarede (original Matlab code)
#' @docType methods
#' @family isotope analysis
#' @export
setGeneric(
  name = "pb_age",
  def = function(x, y, z, ...) standardGeneric("pb_age"),
  valueClass = "data.frame"
)

# Statistics ===================================================================
#' Quantiles of a Density Estimate
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]}.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param ... Currently not used.
#' @return
#'  A `numeric` [`matrix`] containing the quantiles.
#' @example inst/examples/ex-14c-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name quantile
#' @rdname quantile
NULL

#' Mean
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-14c-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mean
#' @rdname mean
NULL

#' Median
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-14c-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name median
#' @rdname median
NULL

# Summary ======================================================================
#' Data Description
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param ... Further parameters to be passed to [cat()].
#' @return
#'  `describe()` is called for its side-effects. Invisibly returns `x`.
#' @references
#'  Millard, A. R. (2014). Conventions for Reporting Radiocarbon Determinations.
#'  *Radiocarbon*, 56(2): 555-559. \doi{10.2458/56.17455}.
#' @example inst/examples/ex-describe.R
#' @author N. Frerebeau
#' @family summary
#' @docType methods
#' @rdname describe
#' @name describe
NULL
