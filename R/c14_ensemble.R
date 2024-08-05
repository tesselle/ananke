# REC
#' @include AllGenerics.R
NULL

#' @export
#' @rdname c14_ensemble
#' @aliases c14_ensemble,CalibratedAges-method
setMethod(
  f = "c14_ensemble",
  signature = "CalibratedAges",
  definition = function(object, from = NULL, to = NULL, by = 10, n = 100,
                        calendar = BP(), progress = getOption("ananke.progress")) {
    ## Check
    c14_validate(object)

    ## Get data
    rd <- aion::time(object, calendar = NULL)
    if (is.null(from)) from <- aion::start(object, calendar = calendar)
    if (is.null(to)) to <- aion::end(object, calendar = calendar)
    grid_years <- seq(from = from, to = to, by = by * sign(to - from))
    grid_rd <- aion::fixed(grid_years, calendar = calendar)

    ## Align 14C date densities onto the grid
    c14_dens <- object[, , 1, drop = TRUE]
    c14_aligned <- apply(
      X = c14_dens,
      MARGIN = 2,
      FUN = function(y, x, grid) {
        fun <- stats::approxfun(x = x, y = y)
        fun(grid)
      },
      x = rd,
      grid = grid_rd
    )
    c14_aligned[is.na(c14_aligned)] <- 0

    ## Build matrix to store the RECE
    count <- matrix(data = 0, nrow = n, ncol = length(grid_rd))
    colnames(count) <- grid_rd

    progress_bar <- progress
    if (progress_bar) pbar <- utils::txtProgressBar(max = n, style = 3)

    n_seq <- seq_len(n)
    for (i in n_seq) {
      ## Sample
      spl <- apply(
        X = c14_aligned,
        MARGIN = 2,
        FUN = function(x, grid) {
          if (sum(x) == 0) return(NA)
          sample(grid, size = 1, prob = x)
        },
        grid = grid_rd
      )
      ## Count
      tbl <- unclass(table(spl)) # Named integer vector
      count[i, names(tbl)] <- tbl
      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }
    count[is.na(count)] <- 0
    if (progress_bar) close(pbar)

    ## Return an RECE object
    ts <- aion::series(
      object = t(count),
      time = grid_rd
      # names = colnames(object)
    )
    .RECE(ts)
  }
)
