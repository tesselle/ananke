# OXCAL

# Dowload ======================================================================
#' Download OxCal
#'
#' @param path A [`character`] string specifying the directory to extract files
#'  to. It will be created if necessary (see [utils::unzip()]).
#' @note
#'  Adapted from [oxcAAR::quickSetupOxcal()].
#' @return Invisibly returns `path`.
#' @example inst/examples/ex-oxcal-execute.R
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_download <- function(path = NULL) {
  ## Set path
  if (is.null(path)) path <- tempdir()
  tmp <- tempfile()
  on.exit(unlink(tmp))

  ## Download
  url <- "https://c14.arch.ox.ac.uk/OxCalDistribution.zip"
  utils::download.file(url, destfile = tmp, quiet = getOption("ananke.progress"))

  ## Extract
  ## TODO: extract only OxCal/bin/
  utils::unzip(tmp, exdir = path)
  msg <- sprintf("OxCal successfully downloaded and extracted to %s.", path)
  message(msg)

  invisible(path)
}

# Setup ========================================================================
#' Setup OxCal
#'
#' @param path A [`character`] string specifying the directory where to find the
#'  OxCal executable (or to extract OxCal files to).
#' @param os A [`character`] string specifying the operating system of the
#'  workstation. It must be one of "`Linux`", "`Windows`" or "`Darwin`".
#'  If `NULL` (the default), the operating system will be determined
#'  automatically (see [Sys.info()]).
#' @param ask A [`logical`] scalar: if OxCal is not installed, should the user
#'  be asked to download it?
#'  If `FALSE` and the OxCal executable cannot be found, will raise an error.
#'  Only used if \R is being used interactively.
#' @details
#'  Downloads the latest version of Oxcal (if needed) and sets the executable
#'  path correctly.
#' @note
#'  Adapted from [oxcAAR::quickSetupOxcal()].
#' @return Invisibly returns the path to the OxCal executable.
#' @example inst/examples/ex-oxcal-execute.R
#' @author C. Schmid, N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_setup <- function(path = NULL, os = NULL, ask = TRUE) {
  ## Set path
  if (is.null(path)) path <- tempdir()

  ## Construct the executable path
  operator <- if (is.null(os)) Sys.info()["sysname"] else os
  binary <- switch(
    operator,
    Linux = "OxCalLinux",
    Windows = "OxCalWin.exe",
    Darwin = "OxCalMac",
    stop(sprintf("Unknown operating system: %s.",
                 sQuote(operator)), call. = FALSE)
  )
  full_path <- file.path(path, "OxCal", "bin", binary)

  ## Validation
  if (!file.exists(full_path)) {
    download <- ""
    if (ask && interactive()) {
      cat(
        "OxCal doesn't seem to be installed.",
        "Do you want to download it?",
        "1. Yes",
        "2. No",
        sep = "\n"
      )
      download <- readline("Choice: ")
      if (download != "1") {
        stop(sprintf("There is no such file: %s.", full_path), call. = FALSE)
      }
    }
    oxcal_download(path)
  }

  ## Set option
  Sys.chmod(full_path, mode = "0777")
  options(ananke.oxcal = full_path)

  invisible(full_path)
}

#' Get OxCal Executable Path
#'
#' @return Returns the path to OxCal executable.
#' @example inst/examples/ex-oxcal-execute.R
#' @author N. Frerebeau
#' @family OxCal tools
#' @keywords internal
#' @noRd
oxcal_path <- function() {
  path <- getOption("ananke.oxcal")
  if (is.null(path) || path == "") {
    stop("Please set the path to OxCal executable.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Please fix the path to OxCal executable.", call. = FALSE)
  }
  path
}

# Execute ======================================================================
#' Execute an Oxcal Script
#'
#' @param script A [`character`] string of instructions for OxCal.
#' @param file A [`character`] string naming a file (without extension) to
#'  write `script` to. Output files will be named after `file` and written to
#'  the same directory.
#' @param timeout An [`integer`] value specifying the timeout in seconds,
#'  ignored if 0. This is a limit for the elapsed time running OxCal.
#'  Fractions of seconds are ignored (see [system2()]).
#' @note
#'  Adapted from [oxcAAR::executeOxcalScript()].
#' @return Invisibly returns a [`list`] of paths to the `output` files.
#' @example inst/examples/ex-oxcal-execute.R
#' @references
#'  \url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_file.html}
#' @author M. Hinz, N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_execute <- function(script, file = NULL, timeout = 0) {
  ## Get OxCal path
  path <- oxcal_path()

  ## Construct output path
  if (is.null(file)) {
    file <- tempfile()
  } else {
    direct <- dirname(file)
    dir.create(direct, showWarnings = FALSE, recursive = TRUE)
  }

  ## Remove existing .work file (if any)
  work <- sprintf("%s.work", file)
  if (file.exists(work)) file.remove(work)

  ## Write script
  oxcal <- sprintf("%s.oxcal", file)
  cat(script, file = oxcal, sep = "\n")

  ## Run OxCal
  out <- suppressWarnings(system2(path, oxcal, wait = TRUE, timeout = timeout))
  if (out == 127L) {
    stop("Something goes wrong...", call. = FALSE)
  }
  if (out == 124L) {
    stop(sprintf("Command terminated after %gs.", timeout), call. = FALSE)
  }

  ## Output files
  output <- list(
    oxcal = oxcal,
    js = sprintf("%s.js", file),
    log = sprintf("%s.log", file),
    txt = sprintf("%s.txt", file),
    csv = NULL
  )

  ## MCMC ?
  csv <- sprintf("%s.csv", file)
  if (file.exists(csv)) {
    output$csv <- csv
  }

  ## Check files
  lapply(
    X = output,
    FUN = function(x) {
      if (!is.null(x) && !file.exists(x)) {
        warning(sprintf("%s does not exist.", x), call. = FALSE)
      }
    }
  )

  results <- structure(output, class = "OxCalFiles")
  invisible(results)
}

# Parse ========================================================================
#' Read and Parse OxCal Output
#'
#' @param file A [`character`] string naming a JavaScript file which the data
#'  are to be read from (typically returned by [oxcal_execute()]).
#' @return A [`list`] with the following elements:
#'  \describe{
#'   \item{`ocd`}{A `list` holding the ranges, probability distributions, etc.
#'   for each parameter.}
#'   \item{`model`}{A `list` containing information about the model.}
#'   \item{`calib`}{A `list` containing information about the calibration
#'   curve.}
#'  }
#' @example inst/examples/ex-oxcal-execute.R
#' @references
#'  \url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_file.html}
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_parse <- function(file) UseMethod("oxcal_parse", file)

#' @rdname oxcal_parse
#' @export
oxcal_parse.OxCalFiles <- function(file) {
  oxcal_parse(file$js)
}

#' @rdname oxcal_parse
#' @export
oxcal_parse.character <- function(file) {
  ox <- V8::v8()
  ox$eval("ocd={};")
  ox$eval("calib={};")
  ox$eval("model={};")
  ox$source(file)

  results <- list(
    ocd = ox$get("ocd"),
    model = ox$get("model"),
    calib = ox$get("calib")
  )

  warn <- results$ocd[[1]]$posterior$warning
  if (!is.null(warn)) warning(warn, call. = FALSE)

  results <- structure(results, class = "OxCalOutput")
  results$ocd <- lapply(X = results$ocd, FUN = structure, class = "OxCalData")

  attr(results, "oxcal") <- results$ocd[[1]]$ref
  attr(results, "curve") <- results$calib[[1]]$ref
  results
}

#' 14C Calibration with OxCal
#'
#' @param names A [`character`] vector specifying the names of the dates (e.g.
#'  laboratory codes).
#' @param dates A [`numeric`] vector giving the BP dates to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the dates
#'  to be calibrated.
#' @param curve A [`character`] string specifying the calibration curve to be
#'  used.
#' @return A [`list`] with the following elements:
#'  \describe{
#'   \item{`ocd`}{A `list` holding the ranges, probability distributions, etc.
#'   for each parameter.}
#'   \item{`model`}{A `list` containing information about the model.}
#'   \item{`calib`}{A `list` containing information about the calibration
#'   curve.}
#'  }
#' @example inst/examples/ex-oxcal-calibrate.R
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_calibrate <- function(names, dates, errors, curve = "IntCal20") {
  ## Validation
  n <- length(names)
  if (length(dates) != n || length(errors) != n) {
    msg <- sprintf("%s, %s and %s must have the same lenght.",
                   sQuote("names"), sQuote("dates"), sQuote("errors"))
    stop(msg, call. = FALSE)
  }
  if (length(curve) != 1) {
    stop("Please select one calibration curve.", call. = FALSE)
  }

  ## OxCal options
  curve <- tolower(curve)
  opt <- sprintf("Options()\n{\nCurve=\"%s.14c\"\n};", curve)

  ## R_Dates
  r_dates <- sprintf("R_Date(\"%s\",%g,%g);", names, dates, errors)
  r_dates <- paste0(r_dates, collapse = "\n")

  ## Execute OxCal
  script <- paste0(c(opt, r_dates), collapse = "\n")
  out <- oxcal_execute(script)

  ## Parse OxCal output
  res <- oxcal_parse(out)
  res
}

# Plot =========================================================================
#' @export
#' @method plot OxCalData
plot.OxCalData <- function(x, likelihood = TRUE, posterior = TRUE,
                           prior = TRUE, curve = TRUE, add = FALSE,
                           ...) {

  plot_density <- function(d, offset = 0, lty = 1, col = "#DDDDDD80") {
    x <- d$x
    y <- d$y + offset
    graphics::polygon(x = c(x, rev(x)), y = c(y, rep(offset, length(y))),
                      col = col, border = NA)
    graphics::lines(x = x, y = y, type = "l", lty = lty, col = "black")
  }

  if (!add) {
    ## Save and restore graphical parameters
    old_par <- graphics::par(mar = c(4, 4, 0, 0) + 0.1, las = 1,
                             no.readonly = TRUE)
    on.exit(graphics::par(old_par))

    graphics::plot(NA, xlim = range(oxcal_range(x), na.rm = TRUE),
                   ylim = c(0, 1), xlab = "Year", ylab = "Probability")
  }

  ## Likelihood
  if (likelihood) {
    like <- oxcal_get_density(x, prob = "likelihood")
    plot_density(like, lty = 1, ...)
  }
  ## Posterior
  if (posterior) {
    like <- oxcal_get_density(x, prob = "posterior")
    plot_density(like, lty = 2, ...)
  }
}

#' @export
#' @method plot OxCalOutput
plot.OxCalOutput <- function(x, decreasing = TRUE, likelihood = TRUE,
                             posterior = TRUE, ...) {

  ## Save and restore graphical parameters
  old_par <- graphics::par(mar = c(4, 0, 0, 0) + 0.1, las = 1,
                           no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  n <- length(x$ocd)
  y <- seq(2, n + 1)
  xlim <- range(oxcal_range(x), na.rm = TRUE)
  graphics::plot(
    NA,
    xlim = xlim,
    ylim = range(y),
    bty = "n",
    xlab = "Year",
    ylab = "",
    yaxt = "n"
  )
  graphics::text(
    x = min(xlim),
    y = y[-1],
    labels = oxcal_get_name(x),
    adj = c(0, 1)
  )

  for (i in seq_len(n)) {
    plot(x$ocd[[i]], likelihood = likelihood, posterior = posterior,
         prior = FALSE, add = TRUE, offset = i)
  }
}

# Print ========================================================================
# @return A [`data.frame`] with the following columns:
#  \describe{
#   \item{`name`}{}
#   \item{`type`}{}
#   \item{`date`}{}
#   \item{`error`}{}
#   \item{`likelihood`}{}
#   \item{`posterior`}{}
#  }
#' @export
#' @method as.data.frame OxCalOutput
as.data.frame.OxCalOutput <- function(x, ...) {
  data.frame(
    name = oxcal_get_name(x),
    operation = oxcal_get_operation(x),
    type = oxcal_get_type(x),
    date = oxcal_get_bp_date(x),
    error = oxcal_get_bp_error(x),
    agreement = oxcal_get_agreement(x),
    convergence = oxcal_get_convergence(x),
    likelihood = I(oxcal_get_density(x, prob = "likelihood")),
    posterior = I(oxcal_get_density(x, prob = "posterior"))
  )
}

#' @export
format.OxCalOutput <- function(x, ...) {
  com <- oxcal_get_comment(x)
  sep <- paste0(rep("-", length.out = getOption("width")), collapse = "")

  paste(com, sep, sep = "\n")
}

#' @export
print.OxCalOutput <- function(x, ...) cat(format(x, ...), sep = "\n")

# Getters ======================================================================
#' @export
oxcal_range <- function(x, ...) UseMethod("oxcal_range")

#' @export
oxcal_range.OxCalOutput <- function(x, na.rm = TRUE, ...) {
  lapply(X = x$ocd, FUN = oxcal_range)
}

#' @export
oxcal_range.OxCalData <- function(x, na.rm = TRUE, ...) {
  start_like <- x$likelihood$start %||% NA_real_
  start_post <- x$posterior$start %||% NA_real_
  if (is.na(start_like) & is.na(start_post)) return(c(NA_real_, NA_real_))

  start <- min(start_like, start_post, na.rm = na.rm)
  stop <- max(
    start_like + x$likelihood$resolution * length(x$likelihood$prob),
    start_post + x$posterior$resolution * length(x$posterior$prob),
    na.rm = na.rm
  )
  c(start, stop)
}

oxcal_get_name <- function(x) UseMethod("oxcal_get_name")
oxcal_get_name.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = character(1), i = "name")
}

oxcal_get_operation <- function(x) UseMethod("oxcal_get_operation")
oxcal_get_operation.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = character(1), i = "op")
}

oxcal_get_type <- function(x) UseMethod("oxcal_get_type")
oxcal_get_type.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = character(1), i = "type")
}

oxcal_get_bp_date <- function(x) UseMethod("oxcal_get_bp_date")
oxcal_get_bp_date.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x[["date"]] %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}

oxcal_get_bp_error <- function(x) UseMethod("oxcal_get_bp_error")
oxcal_get_bp_error.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x[["error"]] %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}

oxcal_get_agreement <- function(x) UseMethod("oxcal_get_agreement")
oxcal_get_agreement.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x$posterior$agreement %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}

oxcal_get_convergence <- function(x) UseMethod("oxcal_get_convergence")
oxcal_get_convergence.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x$posterior$convergence %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}

oxcal_get_curve <- function(x) UseMethod("oxcal_get_curve")
oxcal_get_curve.OxCalOutput <- function(x) {
  years <- seq.int(
    from = x$calib[[1]]$start,
    by = x$calib[[1]]$resolution,
    length.out = length(x$calib[[1]]$bp)
  )
  if (length(years) == 0) return(data.frame())
  data.frame(
    years = years,
    bp = x$calib[[1]]$bp,
    sigma = x$calib[[1]]$sigma
  )
}

#' @export
oxcal_get_density <- function(x, ...) UseMethod("oxcal_get_density")

#' @export
oxcal_get_density.OxCalData <- function(x, prob = c("likelihood", "posterior"), ...) {
  prob <- match.arg(prob, several.ok = FALSE)
  years <- seq.int(
    from = x[[prob]]$start,
    by = x[[prob]]$resolution,
    length.out = length(x[[prob]]$prob)
  )
  if (length(years) == 0) return(list())
  list(x = years, y = x[[prob]]$prob)
}

#' @export
oxcal_get_density.OxCalOutput <- function(x, prob = c("likelihood", "posterior"), ...) {
  lapply(X = x$ocd[-1], FUN = oxcal_get_density, prob = prob)
}

oxcal_get_range <- function(x, ...) UseMethod("oxcal_get_range")
oxcal_get_range.OxCalOutput <- function(x, prob = c("likelihood", "posterior")) {
  prob <- match.arg(prob, several.ok = FALSE)
  lapply(X = x$ocd[-1], FUN = function(x) compact(is.null, x[[prob]]$range))
}

oxcal_get_comment <- function(x, ...) UseMethod("oxcal_get_comment")
oxcal_get_comment.OxCalOutput <- function(x, prob = c("likelihood", "posterior")) {
  prob <- match.arg(prob, several.ok = FALSE)
  vapply(
    X = x$ocd,
    FUN = function(x) {
      com <- x[[prob]]$comment
      paste0(com, collapse = "\n")
    },
    FUN.VALUE = character(1)
  )
}


oxcal_has_likelihood <- function(x) UseMethod("oxcal_has_likelihood")
oxcal_has_likelihood.OxCalOutput <- function(x) {
  like <- oxcal_get_density(x, prob = "likelihood")
  any(lengths(like) > 0)
}
oxcal_has_posterior <- function(x) UseMethod("oxcal_has_posterior")
oxcal_has_posterior.OxCalOutput <- function(x) {
  post <- oxcal_get_density(x, prob = "posterior")
  any(lengths(post) > 0)
}
