# AGE-DEPTH MODELING
#' @include AllGenerics.R
NULL

#' @export
#' @rdname proxy_ensemble
#' @aliases proxy_ensemble,numeric-method
setMethod(
  f = "proxy_ensemble",
  signature = c("numeric"),
  definition = function(positions, proxy_values, proxy_errors, proxy_step,
                        time_values, time_errors, calendar,
                        from = NULL, to = NULL, by = NULL, n = 30,
                        progress = getOption("ananke.progress"),
                        verbose = getOption("ananke.verbose")) {
    ## Validation
    k <- length(positions)
    if (length(proxy_errors) == 1) proxy_errors <- rep(proxy_errors, k)
    arkhe::assert_positive(positions)
    arkhe::assert_decreasing(positions)
    arkhe::assert_length(proxy_values, k)
    arkhe::assert_length(proxy_errors, k)
    arkhe::assert_length(proxy_step, 1)
    arkhe::assert_length(time_values, k)
    arkhe::assert_length(time_errors, k)

    ## Missing values
    if (is.null(from)) from <- time_values[[1L]]
    if (is.null(to))   to <- time_values[[k]]
    if (is.null(by)) {
      grid <- getOption("ananke.grid")
      by <- ((to - from) / (grid - 1))
    }
    if (from > to && by > 0) by <- by * -1

    ## Build a matrix to contain the p(t|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the time density
    if (verbose) cat(tr_("Computing p(t|zi) densities..."), sep = "\n")
    t_grid <- seq(from = from, to = to, by = by)

    t_z <- .mapply(
      FUN = function(min, max, x) {
        stats::dunif(x = x, min = min, max = max)
      },
      dots = list(
        min = time_values - 1 * time_errors,
        max = time_values + 1 * time_errors
      ),
      MoreArgs = list(x = t_grid)
    )
    t_z <- do.call(rbind, t_z)
    t_z <- t(t_z)

    ## Build a matrix to contain the p(x|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the proxy density
    if (verbose) cat(tr_("Computing p(x|zi) densities..."), sep = "\n")
    d <- 2 * max(proxy_errors)
    x_range <- range(c(range(proxy_values) - d, range(proxy_values) + d))
    x_grid <- seq(from = x_range[[1L]], to = x_range[[2L]], by = proxy_step)

    x_z <- .mapply(
      FUN = function(mean, sd, x) {
        stats::dnorm(x = x, mean = mean, sd = sd)
      },
      dots = list(
        mean = proxy_values,
        sd = proxy_errors
      ),
      MoreArgs = list(x = x_grid)
    )
    x_z <- do.call(rbind, x_z)
    x_z <- t(x_z)

    ## Estimate the weighted average density function
    ## (for a given proxy measurement at a given time)
    ## Eq. 4 of Boers et al. 2017
    if (verbose) cat(tr_("Computing p(x|t) densities..."), sep = "\n")

    z <- length(positions)
    ri <- vapply(
      X = 2:(z - 1),
      FUN = function(x, y) (y[x + 1] - y[x - 1]) / 2,
      FUN.VALUE = numeric(1),
      y = positions
    )
    r <- abs(c(positions[2] - positions[1], ri, positions[z] - positions[z - 1]))
    r_t <- matrix(data = r, nrow = nrow(t_z), ncol = ncol(t_z), byrow = TRUE)
    r_x <- matrix(data = r, nrow = nrow(x_z), ncol = ncol(x_z), byrow = TRUE)

    x_t <- t(tcrossprod(r_x * x_z, t_z)) / rowSums(r_t * t_z)

    ## Create an ensemble of potential proxy records
    if (verbose) cat(tr_("Sampling proxy records..."), sep = "\n")
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

    ts <- aion::series(object = Y, time = t_grid, calendar = calendar)
    .ProxyRecord(ts, density = x_t, proxy = x_grid)
  }
)
