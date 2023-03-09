# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HPDI =========================================================================
#' @export
#' @rdname hpdi
#' @aliases hpdi,numeric,numeric-method
setMethod(
  f = "hpdi",
  signature = c(object = "numeric", density = "numeric"),
  definition = function(object, density, level = 0.95) {
    ## Compute density
    x <- object
    y <- density / sum(density)

    ## Order the sample (faster sorting with radix method)
    sorted <- sort(y, decreasing = TRUE, method = "radix")
    i <- min(which(cumsum(sorted) >= sum(y) * level))
    h <- sorted[[i]]
    idx <- which(y >= h)

    gap <- which(diff(idx) > 1)
    inf <- idx[c(1, gap + 1)]
    sup <- idx[c(gap, length(idx))]

    int <- mapply(FUN = seq, from = inf, to = sup,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    p <- vapply(X = int, FUN = function(i, y) { sum(y[i]) },
                FUN.VALUE = numeric(1), y = y)

    # FIXME: reverse boundaries if BP scale?
    cbind(lower = x[inf], upper = x[sup], p = round(p, digits = 2))
  }
)

#' @export
#' @rdname hpdi
#' @aliases hpdi,numeric,missing-method
setMethod(
  f = "hpdi",
  signature = c(object = "numeric", density = "missing"),
  definition = function(object, level = 0.95, ...) {
    ## Compute density
    z <- stats::density(x = object, ...)
    methods::callGeneric(object = z$x, density = z$y, level = level)
  }
)

#' @export
#' @rdname hpdi
#' @aliases hpdi,CalibratedAges,missing-method
setMethod(
  f = "hpdi",
  signature = c(object = "CalibratedAges", density = "missing"),
  definition = function(object, level = 0.95) {
    hpd <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, years, level, CE) hpdi(years, density = x, level = level),
      years = get_years(object),
      level = level,
      simplify = FALSE
    )

    names(hpd) <- names(object)
    attr(hpd, "calendar") <- get_calendar(object)
    hpd
  }
)

#' Simplify
#'
#' Reduces the result of `credible()` or `hpdi()` to a `matrix`.
#' @param x A [`list`] of [`matrix`] (returned by `credible()` or `hpdi()`)
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bind_intervals <- function(x) {
  hpd <- do.call(rbind.data.frame, x)
  rownames(hpd) <- NULL
  n <- vapply(X = x, FUN = nrow, FUN.VALUE = integer(1))
  hpd <- data.frame(name = rep(names(x), times = n), hpd)
  hpd
}
