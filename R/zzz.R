.onLoad <- function(libname, pkgname) {
  op <- options()
  op.chronos <- list(
    chronos.grid = 512,
    chronos.verbose = TRUE,
    chronos.progress = interactive()
  )
  toset <- !(names(op.chronos) %in% names(op))
  if(any(toset)) options(op.chronos[toset])

  invisible()
}
