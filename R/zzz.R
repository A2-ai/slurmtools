.onAttach <- function(libname, pkgname) {
  msg <- slurmtools_options_message()
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  # find squeue version
  # store in options
}
