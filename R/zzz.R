.le <- new.env() # parent = emptyenv()

#' Updates the logging level for functions. Default is set to WARN
#'
#' @param quiet suppresses messaging about log level.
#'
#' @export
#'
#' @examples \dontrun{
#' Sys.setenv("RPFY_VERBOSE" = "DEBUG")
#' toggle_logger()
#' }
toggle_logger <- function(quiet = FALSE) {
  log_level <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  verbosity <- Sys.getenv("SLURMTOOLS_VERBOSE", unset = "WARN")
  if (!(verbosity %in% log_level)) {
    cat(
      "Invalid verbosity level. Available options are:",
      paste(log_level, collapse = ", "),
      "\n"
    )
  }

  logger <- log4r::logger(
    verbosity,
    appenders = log4r::console_appender(my_layout)
  )
  assign("logger", logger, envir = .le)
  if (!quiet) {
    message(paste("logging now at", verbosity, "level"))
  }
}

my_layout <- function(level, ...) {
  paste0(format(Sys.time()), " [", level, "] ", ..., "\n", collapse = "")
}


.onAttach <- function(libname, pkgname) {
  msg <- slurmtools_options_message()
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  result <- tryCatch(
    {
      processx::run(Sys.which("squeue"), args = "--version")
    },
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
      version <- strsplit(result$stdout, " ")[[1]][[2]] %>%
        trimws() %>%
        package_version()
      options("squeue.version" = version)
    }
  }

  toggle_logger(quiet = TRUE)
}
