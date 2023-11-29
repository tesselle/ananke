# F14C <> BP14C
#' @include AllGenerics.R
NULL

#' @export
#' @rdname F14C
#' @aliases BP14C_to_F14C,numeric,numeric-method
setMethod(
  f = "BP14C_to_F14C",
  signature = c(ages = "numeric", errors = "numeric"),
  definition = function(ages, errors, lambda = 8033) {
    values <- exp(ages / -lambda)
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
  definition = function(values, errors, lambda = 8033, asymmetric = FALSE) {
    z <- values

    ## van der Plicht and Hogg 2006, p. 239
    inf_2sigma <- z < 2 * errors
    values[inf_2sigma] <- values[inf_2sigma] + 2 * errors[inf_2sigma]

    inf_zero <- z < 0
    values[inf_zero] <- 2 * errors[inf_zero]

    ## van der Plicht and Hogg 2006, eq. 6
    ## Bronk Ramsey 2008, p. 260
    ages <- -lambda * log(values)
    sigma <- lambda * errors / values

    sigma_plus <- sigma_minus <- sigma
    if (asymmetric) {
      sigma_plus <- - lambda * log(values - errors) - ages
      sigma_minus <- ages + lambda * log(values + errors)
    }

    sigma_plus[inf_zero | inf_2sigma] <- Inf
    sigma_minus[inf_zero | inf_2sigma] <- Inf

    data.frame(age = ages, plus = sigma_plus, minus = sigma_minus)
  }
)
