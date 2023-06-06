# AGE-DEPTH MODELING
#' @include AllGenerics.R
NULL

#' @export
#' @rdname proxy_ensemble
#' @aliases proxy_ensemble,numeric-method
setMethod(
  f = "proxy_ensemble",
  signature = c("numeric"),
  definition = function(depth, proxy, proxy_error, time, time_error, calendar,
                        start = NULL, end = NULL, by = NULL, n = 30,
                        progress = getOption("ananke.progress"),
                        verbose = getOption("ananke.verbose")) {
    ## Validation
    k <- length(depth)
    arkhe::assert_decreasing(depth)
    arkhe::assert_length(proxy, k)
    arkhe::assert_length(time, k)
    arkhe::assert_length(time_error, k)
    if (length(proxy_error) != k) proxy_error <- rep(proxy_error, k)

    ## Missing values
    if (is.null(start)) start <- time[[1L]]
    if (is.null(end))   end <- time[[k]]
    if (is.null(by)) {
      grid <- getOption("ananke.grid")
      by <- ((end - start) / (grid - 1))
    }
    if (start > end && by > 0) by <- by * -1

    ## Build a matrix to contain the p(t|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the time density
    if (verbose) cat("Computing p(t|zi) densities...", sep = "\n")
    t_grid <- seq(from = start, to = end, by = by)

    t_z <- .mapply(
      FUN = function(min, max, x) {
        stats::dunif(x = x, min = min, max = max)
      },
      dots = list(
        min = time - 1 * time_error,
        max = time + 1 * time_error
      ),
      MoreArgs = list(x = t_grid)
    )
    t_z <- do.call(rbind, t_z)
    t_z <- t(t_z)

    ## Build a matrix to contain the p(x|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the proxy density
    if (verbose) cat("Computing p(x|zi) densities...", sep = "\n")
    d <- 2 * max(proxy_error)
    x_grid <- c(min(proxy) - d, sort(unique(proxy)), max(proxy) + d)

    x_z <- .mapply(
      FUN = function(mean, sd, x) {
        stats::dnorm(x = x, mean = mean, sd = sd)
      },
      dots = list(mean = proxy, sd = proxy_error),
      MoreArgs = list(x = x_grid)
    )
    x_z <- do.call(rbind, x_z)
    x_z <- t(x_z)

    ## Estimate the weighted average density function
    ## (for a given proxy measurement at a given time)
    ## Eq. 4 of Boers et al. 2017
    if (verbose) cat("Computing p(x|t) densities...", sep = "\n")

    z <- length(depth)
    ri <- vapply(
      X = 2:(z - 1),
      FUN = function(x, y) (y[x + 1] - y[x - 1]) / 2,
      FUN.VALUE = numeric(1),
      y = depth
    )
    r <- abs(c(depth[2] - depth[1], ri, depth[z] - depth[z - 1]))
    r_t <- matrix(data = r, nrow = nrow(t_z), ncol = ncol(t_z), byrow = TRUE)
    r_x <- matrix(data = r, nrow = nrow(x_z), ncol = ncol(x_z), byrow = TRUE)

    x_t <- t(tcrossprod(r_x * x_z, t_z)) / rowSums(r_t * t_z)

    ## Create an ensemble of potential proxy records
    if (verbose) cat("Sampling proxy records...", sep = "\n")
    Y <- matrix(data = 0, nrow = length(t_grid), ncol = n)

    if (progress) pb <- utils::txtProgressBar(min = 0, max = n, style = 3)

    n_seq <- seq_len(n)
    for (i in n_seq) {
      ## Sample
      Y[, i] <- apply(
        X = x_t,
        MARGIN = 1,
        FUN = function(x, g) sample(g, size = 1, prob = x),
        g = x_grid
      )
      if (progress) utils::setTxtProgressBar(pb, i)
    }

    if (progress) close(pb)

    time_series <- aion::series(
      object = Y,
      time = t_grid,
      calendar = calendar
    )
    .ProxyRecord(
      time_series,
      density = x_t,
      proxy = x_grid
    )
  }
)

#' @export
#' @method plot ProxyRecord
plot.ProxyRecord <- function(x, calendar = getOption("ananke.calendar"), ...) {
  ## Get data
  years <- aion::time(x, calendar = NULL)
  z <- apply(
    X = x@density,
    MARGIN = 1,
    FUN = function(d) (d - min(d)) / max(d - min(d)) * 1.5
  )
  z[z == 0] <- NA

  ## Plot
  graphics::image(x = years, y = x@proxy, z = t(z),
                  xlab = format(calendar), ylab = "Proxy",
                  xaxt = "n", yaxt = "n", ...)

  ## Construct axes
  year_axis(x = years, side = 1, format = TRUE, calendar = calendar)
  graphics::axis(side = 2, las = 1)

  m <- apply(
    X = x@density,
    MARGIN = 1,
    FUN = function(w, x, na.rm) stats::weighted.mean(x = x, w = w),
    x = x@proxy
  )
  graphics::lines(x = years, y = m, col = "black", lwd = 2)

  invisible(x)
}

#' @export
#' @rdname proxy_plot
#' @aliases plot,ProxyRecord,missing-method
setMethod("plot", c(x = "ProxyRecord", y = "missing"), plot.ProxyRecord)
