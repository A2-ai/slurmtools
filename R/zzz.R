.onAttach <- function(libname, pkgname) {
  msg <- slurmtools_options_message()
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  processx_output <- processx::run("squeue", args = "--version")
  if (processx_output$status != 0) {
    warning("squeue not installed correctly")
  } else {
    # stdout format is "slurm major.minor.patch/n"
    version <- strsplit(processx_output$stdout," ")[[1]][[2]] %>%
      trimws() %>%
      package_version()
    options("squeue.version" = version)
  }
}
