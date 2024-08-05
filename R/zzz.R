.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ananke <- list(
    ananke.calendar = aion::CE(),
    ananke.grid = 512,
    ananke.verbose = interactive(),
    ananke.progress = interactive()
  )
  toset <- !(names(op.ananke) %in% names(op))
  if(any(toset)) options(op.ananke[toset])

  invisible()
}
