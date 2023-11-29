# 14C SPD
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_spd
#' @aliases c14_spd,CalibratedAges-method
setMethod(
  f = "c14_spd",
  signature = "CalibratedAges",
  definition = function(object, normalize_date = FALSE, normalize_spd = FALSE) {
    ## Check
    calibrate_check(colnames(object), object@status)

    dens <- t(object[, , 1, drop = TRUE])
    if (normalize_date) dens <- dens / rowSums(dens, na.rm = TRUE)
    spd <- colSums(dens, na.rm = TRUE)
    if (normalize_spd) spd <- spd / sum(spd, na.rm = TRUE)

    time_series <- aion::series(
      object = spd,
      time = aion::time(object, calendar = NULL)
    )
    .CalibratedSPD(time_series)
  }
)
