.onLoad <- function(libname, pkgname) {
  ## Set package options
  op <- options()
  op.ananke <- list(
    ananke.progress = interactive(),
    ananke.oxcal = ""
  )
  toset <- !(names(op.ananke) %in% names(op))
  if(any(toset)) options(op.ananke[toset])

  invisible()
}
