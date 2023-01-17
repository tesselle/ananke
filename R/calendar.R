# TIME SCALES
#' @include AllGenerics.R
NULL

# Predicates ===================================================================
## BP --------------------------------------------------------------------------
#' @export
#' @rdname BP
#' @aliases is_BP,CalibratedAges-method
setMethod(
  f = "is_BP",
  signature = "CalibratedAges",
  definition = function(object) identical(get_calendar(object), "BP")
)

## CE --------------------------------------------------------------------------
#' @export
#' @rdname CE
#' @aliases is_CE,CalibratedAges-method
setMethod(
  f = "is_CE",
  signature = "CalibratedAges",
  definition = function(object) identical(get_calendar(object), "CE")
)

## b2k -------------------------------------------------------------------------
#' @export
#' @rdname b2k
#' @aliases is_b2k,CalibratedAges-method
setMethod(
  f = "is_b2k",
  signature = "CalibratedAges",
  definition = function(object) identical(get_calendar(object), "b2k")
)

# Convert ======================================================================
## BP to CE --------------------------------------------------------------------
#' @export
#' @rdname BP
#' @aliases BP_to_CE,numeric-method
setMethod(
  f = "BP_to_CE",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] < 0)) {
      stop("Post-bomb dates (< 0 BP) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object < 1950] <- 1950 - object[index & object < 1950]
    tmp[index & object >= 1950] <- 1949 - object[index & object >= 1950]
    tmp
  }
)

#' @export
#' @rdname BP
#' @aliases BP_to_CE,matrix-method
setMethod(
  f = "BP_to_CE",
  signature = "matrix",
  definition = function(object){
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname BP
#' @aliases BP_to_CE,array-method
setMethod(
  f = "BP_to_CE",
  signature = "array",
  definition = function(object){
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

## BP to b2k -------------------------------------------------------------------
#' @export
#' @rdname BP
#' @aliases BP_to_b2k,numeric-method
setMethod(
  f = "BP_to_b2k",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    tmp <- rep(NA, length(object))
    tmp[index] <- object[index] + 50
    tmp
  }
)

#' @export
#' @rdname BP
#' @aliases BP_to_b2k,matrix-method
setMethod(
  f = "BP_to_b2k",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname BP
#' @aliases BP_to_b2k,array-method
setMethod(
  f = "BP_to_b2k",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

## CE to BP --------------------------------------------------------------------
#' @export
#' @rdname CE
#' @aliases CE_to_BP,numeric-method
setMethod(
  f = "CE_to_BP",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] == 0)) {
      stop("0 BCE/CE is not a valid year!", call. = FALSE)
    }
    if (any(object[index] > 1950)) {
      stop("Post-bomb dates (> 1950 CE) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object > 0] <- abs(object[index & object > 0] - 1950)
    tmp[index & object < 0] <- abs(object[index & object < 0] - 1949)
    tmp
  }
)

#' @export
#' @rdname CE
#' @aliases CE_to_BP,matrix-method
setMethod(
  f = "CE_to_BP",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname CE
#' @aliases CE_to_BP,array-method
setMethod(
  f = "CE_to_BP",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

## CE to b2k -------------------------------------------------------------------
#' @export
#' @rdname CE
#' @aliases CE_to_b2k,numeric-method
setMethod(
  f = "CE_to_b2k",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] == 0)) {
      stop("0 BCE/CE is not a valid year!", call. = FALSE)
    }
    if (any(object[index] > 2000)) {
      stop("Actual dates (> 2000 CE) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object > 0] <- abs(object[index & object > 0] - 2000)
    tmp[index & object < 0] <- abs(object[index & object < 0] - 1999)
    tmp
  }
)

#' @export
#' @rdname CE
#' @aliases CE_to_b2k,matrix-method
setMethod(
  f = "CE_to_b2k",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname CE
#' @aliases CE_to_b2k,array-method
setMethod(
  f = "CE_to_b2k",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

## b2k to BP -------------------------------------------------------------------
#' @export
#' @rdname b2k
#' @aliases b2k_to_BP,numeric-method
setMethod(
  f = "b2k_to_BP",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    tmp <- rep(NA, length(object))
    tmp[index] <- object[index] - 50
    tmp
  }
)

#' @export
#' @rdname b2k
#' @aliases b2k_to_BP,matrix-method
setMethod(
  f = "b2k_to_BP",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname b2k
#' @aliases b2k_to_BP,array-method
setMethod(
  f = "b2k_to_BP",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

## b2k to CE -------------------------------------------------------------------
#' @export
#' @rdname b2k
#' @aliases b2k_to_CE,numeric-method
setMethod(
  f = "b2k_to_CE",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] < 0)) {
      stop("Actual dates (< 0 b2k) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object < 2000] <- 2000 - object[index & object < 2000]
    tmp[index & object >= 2000] <- 1999 - object[index & object >= 1999]
    tmp
  }
)

#' @export
#' @rdname b2k
#' @aliases b2k_to_CE,matrix-method
setMethod(
  f = "b2k_to_CE",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname b2k
#' @aliases b2k_to_CE,array-method
setMethod(
  f = "b2k_to_CE",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)
