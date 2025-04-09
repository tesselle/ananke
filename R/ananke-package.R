#' @details
#'  \tabular{ll}{
#'   **Package:** \tab ananke \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.1.0 \cr
#'   **License:** \tab GPL-3 \cr
#'  }
#'
#' @section Package options:
#'  \pkg{ananke} uses the following [options()] to configure behavior:
#'  * `ananke.grid`: a [`numeric`] value specifying the number of equally
#'    spaced points at which densities are to be estimated (defaults to
#'    \eqn{512}). Should be a power of \eqn{2} (see [stats::density()]).
#'  * `ananke.round`: a [`character`] string specifying the rounding convention.
#'    It can be one of "`none`" (the default, no rounding) or "`stuiver`"
#'    (Stuiver & Polach, 1977).
#'  * `ananke.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed? Defaults to [interactive()].
#'  * `ananke.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to [interactive()].
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order):
#'
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'  }
#'
#'  **Package maintainer**
#'
#'  Nicolas Frerebeau\cr
#'  \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#'  Archéosciences Bordeaux (UMR 6034)\cr
#'  Maison de l'Archéologie\cr
#'  Université Bordeaux Montaigne\cr
#'  F-33607 Pessac cedex\cr
#'  France
#' @name ananke-package
#' @aliases ananke
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @import aion
#' @importFrom methods as new setGeneric setMethod setValidity .valueClassTest
NULL
