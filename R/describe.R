# DESCRIBE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname describe
#' @aliases describe,CalibratedAges-method
setMethod(
  f = "describe",
  signature = signature(x = "CalibratedAges"),
  definition = function(x, calendar = getOption("ananke.calendar"), level = 0.954) {
    ## Get data
    lab <- labels(x)
    val <- x@values
    err <- x@errors
    crv <- x@curves
    reservoir_off <- x@reservoir_offsets
    reservoir_err <- x@reservoir_errors
    F14C <- x@F14C

    ## Laboratory code
    if (F14C) {
      txt_uncal <- "Sample %s contains %.0f +/- %.0f F14C,"
    } else {
      txt_uncal <- "Sample %s is dated to %.0f +/- %.0f BP,"
    }
    msg_uncal <- sprintf(txt_uncal, lab, val, err)

    ## Calibration results
    hdr <- interval_hdr(x, level = level, calendar = calendar)
    msg_cal <- lapply(
      X = hdr,
      FUN = function(x, calendar, level) {
        if (is.null(x)) return("but is out of the calibration range of")
        p <- if (NROW(x) > 1) sprintf(" (%.1f%%)", x[, 3] * 100) else ""
        msg_hdr <- sprintf("[%.0f,%.0f]%s", x[, 1], x[, 2], p)
        txt_cal <- "calibrated to %s %s (%.1f%% HPD interval) with"
        sprintf(txt_cal, paste0(msg_hdr, collapse = " or "), calendar, level)
      },
      calendar = calendar@label,
      level = level * 100
    )

    ## Calibration curve
    txt_curve <- "%s (%s)."
    msg_curve <- sprintf(txt_curve, crv, cite_curve(crv))

    ## Text
    msg <- paste(msg_uncal, msg_cal, msg_curve, sep = " ")

    ## Software
    txt_soft <- "Calibration was computed with R %s.%s (R Core Team %s) and package ananke %s (Frerebeau %s)."
    date_soft <- utils::packageDate("ananke")
    date_soft <- if (is.na(date_soft)) Sys.Date() else date_soft
    msg_soft <- sprintf(txt_soft, R.version$major, R.version$minor,
                        R.version$year, utils::packageVersion("ananke"),
                        format(date_soft, format = "%Y"))

    cat(unlist(msg), msg_soft, sep = "\n\n")

    invisible(x)
  }
)
