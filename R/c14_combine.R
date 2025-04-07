# COMBINE 14C
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_combine
#' @aliases c14_combine,numeric,numeric-method
setMethod(
  f = "c14_combine",
  signature = c(values = "numeric", errors = "numeric"),
  definition = function(values, errors, groups = NULL) {
    ## Validation
    n <- length(values)
    if (is.null(groups)) groups <- "X"
    if (length(groups) == 1) groups <- rep(groups, n)
    groups <- factor(x = groups, levels = unique(groups))

    arkhe::assert_missing(values)
    arkhe::assert_missing(errors)
    arkhe::assert_length(errors, n)
    arkhe::assert_length(groups, n)

    ## Empty groups must be treated as NA
    groups[groups == ""] <- NA

    ## Groups with only one date must be treated as NA
    counts <- table(groups)
    one <- groups %in% names(counts)[counts == 1]

    # NA group will be removed
    # We need to keep isolated dates
    k <- one | is.na(groups)
    solo <- NULL
    if (any(k)) {
      solo <- data.frame(
        groups = as.character(groups[k]),
        values = values[k],
        errors = errors[k],
        chi2 = NA_real_,
        p = NA_real_
      )
    }

    combined <- NULL
    if (!all(k)) {
      groups[k] <- NA
      groups <- droplevels(groups)

      ## split() removes NA group
      values <- split(values, f = groups)
      errors <- split(errors, f = groups)
      cmbn <- mapply(
        FUN = combine,
        values = values,
        errors = errors,
        SIMPLIFY = FALSE
      )
      combined <- data.frame(names(cmbn), do.call(rbind, cmbn))
      colnames(combined) <- c("groups", "values", "errors", "chi2", "p")
    }

    final <- rbind(solo, combined, make.row.names = FALSE)
    final
  }
)

combine <- function(values, errors) {
  ## On calcule la moyenne pondérée
  w <- 1 / errors^2 # Facteur de pondération
  moy <- stats::weighted.mean(x = values, w = w)

  ## On calcule l'incertitude associée à la moyenne pondérée
  err <- sum(1 / errors^2)^(-1 / 2)

  ## On calcule la statistique du test
  chi2 <- sum(((values - moy) / errors)^2)

  ## On calcule la valeur-p
  p <- 1 - stats::pchisq(chi2, df = length(values))

  ## On stocke les résultats
  c(moy, err, chi2, p)
}
