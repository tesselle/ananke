#' @details
#'  \tabular{ll}{
#'   **Package:** \tab ananke \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.0.0 \cr
#'   **License:** \tab GPL-3 \cr
#'  }
#'
#' @section Package options:
#'  `ananke` uses the following [options()] to configure behavior:
#'  * `ananke.calendar`: an [`aion::TimeScale-class`] object (default calendar
#'    for printing; see [aion::calendar()]).
#'  * `ananke.grid`: a [`numeric`] value specifying the number of equally
#'    spaced points at which densities are to be estimated (defaults to
#'    \eqn{512}). Should be a power of \eqn{2} (see [stats::density()]).
#'  * `ananke.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed?
#'  * `ananke.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to `TRUE`.
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
