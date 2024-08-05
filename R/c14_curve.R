# 14C CALIBRATION CURVE
#' @include AllGenerics.R
NULL

# Get curve ====================================================================
#' @export
#' @rdname c14_curve
#' @aliases c14_curve,character-method
setMethod(
  f = "c14_curve",
  signature = c(name = "character"),
  definition = function(name) {
    curve_ok <- c("intcal20", "intcal13", "intcal09", "intcal04",
                  "marine20", "marine13", "marine09", "marine04")
    name <- match.arg(name, choices = curve_ok, several.ok = TRUE)

    curves <- lapply(X = name, FUN = read_curve)
    names(curves) <- name
    curves
  }
)

#' @export
#' @rdname c14_curve
#' @aliases c14_curve,CalibratedAges-method
setMethod(
  f = "c14_curve",
  signature = c(name = "CalibratedAges"),
  definition = function(name) {
    methods::callGeneric(name@curves)
  }
)

read_curve <- function(x) {
  ## Read data
  curve_dir <- system.file("extdata", package = "ananke")
  curve_path <- file.path(curve_dir, paste0(x, ".14c"))
  curve <- utils::read.table(curve_path, header = FALSE, sep = ",",
                             dec = ".", strip.white = TRUE,
                             comment.char = "#")

  curve <- curve[, c(1, 2, 3)]
  colnames(curve) <- c("CALBP", "AGE", "ERROR")
  curve
}

cite_curve <- function(x) {
  curve <- c(
    bomb04nh1 = "Hua and Barbetti 2004",
    bomb04nh2 = "Hua and Barbetti 2004",
    bomb04nh3 = "Hua and Barbetti 2004",
    bomb04sh = "Hua and Barbetti 2004",
    bomb13nh1 = "Hua, Berbetti and Rakowski 2013",
    bomb13nh2 = "Hua, Berbetti and Rakowski 2013",
    bomb13nh3 = "Hua, Berbetti and Rakowski 2013",
    bomb13sh12 = "Hua, Berbetti and Rakowski 2013",
    bomb13sh3 = "Hua, Berbetti and Rakowski 2013",
    bomb21nh1 = "Hua et al. 2022",
    bomb21nh2 = "Hua et al. 2022",
    bomb21nh3 = "Hua et al. 2022",
    bomb21sh12 = "Hua et al. 2022",
    bomb21sh3 = "Hua et al. 2022",
    cariaco04 = "Hughen et al. 2004",
    intcal98 = "Stuiver et al. 1998",
    intcal04 = "Reimer et al. 2004",
    intcal09 = "Reimer et al. 2009",
    intcal13 = "Reimer et al. 2013",
    intcal20 = "Reimer et al. 2020",
    kueppers04 = "Kueppers et al. 2004",
    marine98 = "Stuiver, Reimer and Braziunas 1998",
    marine04 = "Hughen et al. 2004",
    marine09 = "Reimer et al. 2009",
    marine13 = "Reimer et al. 2013",
    marine20 = "Heaton et al. 2020",
    shcal04 = "McCormac et al. 2004",
    shcal13 = "Hogg et al. 2013",
    shcal20 = "Hogg et al. 2020"
  )

  curve[x]
}

# Approximate curve ============================================================
#' Interpolate 14C Calibration Curve
#'
#' @param name A [`character`] vector naming calibration curves.
#' @param out A [`numeric`] vector specifying where interpolation is to take
#'  place.
#' @param F14C A [`logical`] scalar: should estimated F14C values be used
#'  instead of radiocarbon ages?
#' @return
#'  A `list` of `list` with the following elements:
#'  \tabular{ll}{
#'  `mu`  \tab Interpolated values                    \cr
#'  `tau` \tab Interpolated errors                    \cr
#'  `max` \tab Maximum value of the calibration curve \cr
#'  `min` \tab Minimum value of the calibration curve \cr
#'  }
#' @keywords internal
#' @noRd
approx_curve <- function(name, out, F14C = FALSE) {
  ## Get data
  curve_data <- c14_curve(name)

  ## Interpolate
  lapply(
    X = curve_data,
    FUN = function(x, xout, F14C) {
      if (F14C) {
        x_f14c <- BP14C_to_F14C(x[, 2], x[, 3])
        x[, 2] <- x_f14c$value
        x[, 3] <- x_f14c$error
      }

      list(
        mu = stats::approx(x[, 1], x[, 2], xout = xout)$y,
        tau = stats::approx(x[, 1], x[, 3], xout = xout)$y,
        max = max(x[, 2]),
        min = min(x[, 2])
      )
    },
    xout = out,
    F14C = F14C
  )
}
