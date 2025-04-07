# DESCRIBE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname describe
#' @aliases describe,CalibratedAges-method
setMethod(
  f = "describe",
  signature = signature(x = "CalibratedAges"),
  definition = function(x, calendar = get_calendar(),
                        level = 0.954, ...) {
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
    hdr <- as.list(interval_hdr(x, level = level), calendar = calendar)
    msg_cal <- lapply(
      X = hdr,
      FUN = function(x, calendar, level) {
        if (is.null(x)) return("but is out of the calibration range of")
        p <- if (NROW(x) > 1) sprintf(" (%.1f%%)", x$p * 100) else ""
        msg_hdr <- sprintf("[%.0f,%.0f]%s", x$start, x$end, p)
        txt_cal <- "calibrated to %s %s (%.1f%% HPD interval) with"
        sprintf(txt_cal, paste0(msg_hdr, collapse = " or "), calendar, level)
      },
      calendar = calendar@label,
      level = level * 100
    )

    ## Calibration curve
    txt_marine <- "marine reservoir offset: %.0f +/- %.0f; "
    msg_marine <- sprintf(txt_marine, reservoir_off, reservoir_err)
    msg_marine[reservoir_off == 0] <- ""
    txt_curve <- "%s (%s%s)."
    msg_curve <- sprintf(txt_curve, crv, msg_marine, cite_curve(crv))

    ## Text
    msg <- paste(msg_uncal, msg_cal, msg_curve, sep = " ")

    ## Software
    txt_soft <- "Calibration was computed with R %s.%s (R Core Team %s) and package ananke %s (Frerebeau %s)."
    date_soft <- utils::packageDate("ananke")
    date_soft <- if (is.na(date_soft)) Sys.Date() else date_soft
    msg_soft <- sprintf(txt_soft, R.version$major, R.version$minor,
                        R.version$year, utils::packageVersion("ananke"),
                        format(date_soft, format = "%Y"))

    ## Split string
    fill <- list(...)$fill
    if (!is.null(fill) && isTRUE(fill)) {
      msg <- lapply(
        X = msg,
        FUN = function(x, width) {
          split <- lapply(
            X = seq(from = 1, to = nchar(x), by = width),
            FUN = function(i) substr(x, i, i + width - 1)
          )
          paste0(split, collapse = "\n")
        },
        width = getOption("width")
      )
    }

    cat(unlist(msg), msg_soft, sep = "\n\n", ...)

    invisible(x)
  }
)
