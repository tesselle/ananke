# F14C <> BP14C
#' @include AllGenerics.R
NULL

#' @export
#' @rdname F14C
#' @aliases BP14C_to_F14C,numeric,numeric-method
setMethod(
  f = "BP14C_to_F14C",
  signature = c(values = "numeric", errors = "numeric"),
  definition = function(values, errors, lambda = 8033) {
    values <- exp(values / -lambda)
    sigma <- values * errors / lambda
    data.frame(value = values, error = sigma)
  }
)

#' @export
#' @rdname F14C
#' @aliases F14C_to_BP14C,numeric,numeric-method
setMethod(
  f = "F14C_to_BP14C",
  signature = c(values = "numeric", errors = "numeric"),
  definition = function(values, errors, lambda = 8033, asymmetric = FALSE,
                        rounding = getOption("ananke.round")) {
    ## Validation
    choices <- c("none", "stuiver")
    rounding <- match.arg(rounding, choices = choices, several.ok = FALSE)

    z <- values

    ## van der Plicht and Hogg 2006, p. 239
    below_2sigma <- z < 2 * errors
    values[below_2sigma] <- values[below_2sigma] + 2 * errors[below_2sigma]

    below_zero <- z < 0
    values[below_zero] <- 2 * errors[below_zero]

    ## van der Plicht and Hogg 2006, eq. 6
    ## Bronk Ramsey 2008, p. 260
    ages <- -lambda * log(values)
    sigma <- lambda * errors / values

    sigma_plus <- sigma_minus <- sigma
    if (asymmetric) {
      sigma_plus <- - lambda * log(values - errors) - ages
      sigma_minus <- ages + lambda * log(values + errors)
    }

    sigma_plus[below_zero | below_2sigma] <- NA_real_
    sigma_minus[below_zero | below_2sigma] <- NA_real_

    ## The reported age can be no older than the radiocarbon age of the
    ## fraction modern of the sample plus it's error
    # limit <- -lambda * log(values + errors)
    # ages[ages > limit] <- limit[ages > limit]

    ## Rounding
    if (identical(rounding, "stuiver")) {
      ages <- round_values_stuiver(ages)
      sigma_plus <- round_errors_stuiver(sigma_plus)
      sigma_minus <- round_errors_stuiver(sigma_minus)
    }

    data.frame(age = ages, plus = sigma_plus, minus = sigma_minus)
  }
)

## Stuiver & Polach (1977), p. 362
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
round_values_stuiver <- function(x) {
  y <- x[!is.na(x)]
  y[y < 1000] <- round_any(y[y < 1000], 5)
  y[1000 <= y & y <= 9999] <- round_any(y[1000 <= y & y < 9999], 10)
  y[10000 <= y & y <= 20000] <- round_any(y[10000 <= y & y <= 20000], 50)
  y[y > 20000] <- round_any(y[y > 20000], 100)
  x[!is.na(x)] <- y
  x
}
round_errors_stuiver <- function(x) {
  y <- x[!is.na(x)]
  y[y < 50] <- round_any(y[y < 50], 5)
  y[50 <= y & y <= 1000] <- round_any(y[50 <= y & y <= 1000], 10)
  y[y > 1000] <- round_any(y[y > 1000], 100)
  x[!is.na(x)] <- y
  x
}
