.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ananke <- list(
    ananke.grid = 512,
    ananke.progress = interactive(),
    ananke.round = "none",
    ananke.verbose = interactive()
  )
  toset <- !(names(op.ananke) %in% names(op))
  if(any(toset)) options(op.ananke[toset])

  invisible()
}
