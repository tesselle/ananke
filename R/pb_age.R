# GEOLOGICAL MODEL AGE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname pb_age
#' @aliases pb_age,numeric,numeric,numeric-method
setMethod(
  f = "pb_age",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, t0 = 3.8,
                        x_star = 18.75, y_star = 15.63, z_star = 38.86,
                        mu = 9.66, kappa = 3.90, th232 = 0.049475,
                        u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79,
                        tolerance = sqrt(.Machine$double.eps),
                        stop = 100) {
    ## Validation
    n <- length(x)
    arkhe::assert_length(y, n)
    arkhe::assert_length(z, n)

    x0 <- x_star - mu * (exp(u238 * t0) - 1) # 206Pb/204Pb at t0
    y0 <- y_star - mu / u238_235 * (exp(u235 * t0) - 1) # 207Pb/204Pb at t0
    z0 <- z_star - mu * kappa * (exp(th232 * t0) - 1) # 208Pb/204Pb at t0

    t_model <- vector(mode = "numeric", length = n)
    f_x <- vector(mode = "numeric", length = n)
    for (i in seq_along(t_model)) {
      tt <- 0
      f <- 1

      iter <- 0
      while (abs(f) > tolerance) { # Newton loop
        slope <- (y[i] - y0) / (x[i] - x0)
        u <- exp(u235 * t0) - exp(u235 * tt)
        v <- exp(u238 * t0) - exp(u238 * tt)
        uprime <- -u235 * exp(u235 * tt)
        vprime <- -u238 * exp(u238 * tt)

        f <- u / v - slope * u238_235
        df <- (1 / v) * (uprime - u / v * vprime) # df/dtt
        dtt <- f / df # Newton-Raphson increment
        tt <- tt - dtt

        # Loop counter
        iter <- iter + 1
        if (iter >= stop) {
          msg <- tr_("Convergence not reached (possible infinite loop).")
          warning(msg, call. = FALSE)
          break
        }
      }

      t_model[i] <- tt
      f_x[i] <- f # Residual (should be zero)
    }

    mu_i <- (x - x0) / (exp(u238 * t0) - exp(u238 * t_model))
    kappa_i <- (z - z0) / (exp(th232 * t0) - exp(th232 * t_model)) / mu_i

    data.frame(
      age = t_model * 1000, # Use Ma instead of Ga
      mu = mu_i,
      kappa = kappa_i,
      residual = f_x
    )
  }
)

#' @export
#' @rdname pb_age
#' @aliases pb_age,list,missing,missing-method
setMethod(
  f = "pb_age",
  signature = c(x = "list", y = "missing", z = "missing"),
  definition = function(x, t0 = 3.8,
                        x_star = 18.75, y_star = 15.63, z_star = 38.86,
                        mu = 9.66, kappa = 3.90, th232 = 0.049475,
                        u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79,
                        tolerance = sqrt(.Machine$double.eps), stop = 100) {
    ## Validation
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, t0 = t0,
                         x_star = x_star, y_star = y_star, z_star = z_star,
                         mu = mu, kappa = kappa, th232 = th232,
                         u238 = u238, u235 = u235, u238_235 = u238_235,
                         tolerance = tolerance, stop = stop)
  }
)
