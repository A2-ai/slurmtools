.onAttach <- function(libname, pkgname) {
  msg <- slurmtools_options_message()
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  result <- tryCatch(
    {processx::run("squeue", args = "--version")},
    error = function(e) {
      warning("'squeue' command not found or failed to run.")
      return(NULL)
    }
  )

  if (!is.null(result)) {
    if (result$status != 0) {
      warning("squeue not installed correctly")
    } else {
      # stdout format is "slurm major.minor.patch/n"
      version <- strsplit(result$stdout," ")[[1]][[2]] %>%
        trimws() %>%
        package_version()
      options("squeue.version" = version)
    }
  }
}
