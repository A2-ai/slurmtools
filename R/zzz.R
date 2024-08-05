.onAttach <- function(libname, pkgname) {
  msg <- slurmtools_options_message()
  packageStartupMessage(msg)
}
