#' Generates a tidyverse-esque onAttach message
#'
#' @return a message to display on attach
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' slurmtools_options_message()
#' }
slurmtools_options_message <- function() {
  set_options <- c()
  unset_options <- c()

  # Check for each used option
  root <- getOption('slurmtools.submission_root')
  if (is.null(root)) {
    unset_options <- c(
      unset_options,
      "options('slurmtools.submission_root') is not set."
    )
  } else {
    set_options <- c(set_options, paste("slurmtools.submission_root:", root))
  }

  #format .onAttach message
  msg <- "\n\n"
  if (length(set_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Set slurmtools options")
      ),
      "\n",
      paste0(
        cli::col_green(cli::symbol$tick),
        " ",
        set_options,
        collapse = "\n"
      ),
      "\n"
    )
  }

  if (length(unset_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Needed slurmtools options")
      ),
      "\n",
      paste0(
        cli::col_red(cli::symbol$cross),
        " ",
        unset_options,
        collapse = "\n"
      ),
      "\n",
      paste0(
        cli::col_cyan(cli::symbol$info),
        " ",
        cli::format_inline(
          "Please set all options for job submission defaults to work."
        )
      )
    )
  }

  paste0(msg, "\n\n")
}
