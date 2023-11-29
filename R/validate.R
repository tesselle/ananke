# CLASSES VALIDATION
#' @include AllClasses.R
NULL

setValidity(
  Class = "CalibratedAges",
  method = function(object) {
    ## Get data
    values <- object@values
    errors <- object@errors
    curves <- object@curves
    F14C <- object@F14C
    status <- object@status

    p <- ncol(object)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_length(values, p)),
      arkhe::validate(arkhe::assert_length(errors, p)),
      arkhe::validate(arkhe::assert_length(curves, p)),
      arkhe::validate(arkhe::assert_length(F14C, 1)),
      arkhe::validate(arkhe::assert_length(status, p))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

# setValidity(
#   Class = "ProxyRecord",
#   method = function(object) {
#     ## Get data
#     depth <- object@depth
#     proxy <- object@proxy
#     proxy_error <- object@proxy_error
#     time <- object@time
#     time_error <- object@time_error
#     m <- nrow(object)
#
#     ## Validate
#     cnd <- list(
#       arkhe::validate(arkhe::assert_type(depth, "numeric")),
#       arkhe::validate(arkhe::assert_length(depth, m)),
#       arkhe::validate(arkhe::assert_type(proxy, "numeric")),
#       arkhe::validate(arkhe::assert_length(proxy, m)),
#       arkhe::validate(arkhe::assert_type(proxy_error, "numeric")),
#       arkhe::validate(arkhe::assert_length(proxy_error, m)),
#       arkhe::validate(arkhe::assert_type(time, "numeric")),
#       arkhe::validate(arkhe::assert_length(time, m)),
#       arkhe::validate(arkhe::assert_type(time_error, "numeric")),
#       arkhe::validate(arkhe::assert_length(time_error, m))
#     )
#
#     ## Return conditions, if any
#     arkhe::check_class(object, cnd)
#   }
# )
