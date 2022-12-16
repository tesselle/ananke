#' @details
#' \tabular{ll}{
#'  **Package:** \tab ananke \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 0.0.0 \cr
#'  **License:** \tab GPL-3 \cr
#' }
#'
#' @section Package options:
#'  `ananke` uses the following [options()] to configure behavior:
#'  * `ananke.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed?
#'  * `ananke.oxcal`: a [`character`] string specifying the path to the OxCal
#'    executable.
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#' }
#'
#' **Package maintainer**
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' 33607 Pessac cedex\cr
#' France
#' @name ananke-package
#' @aliases ananke
#' @docType package
#' @keywords internal
"_PACKAGE"

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}
compact <- function(f, x) {
  Filter(Negate(f), x)
}
