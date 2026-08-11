#' Generates a tidyverse-esque onAttach message
#'
#' Nothing is required for submission to work: `submission_root` falls back to
#' `"submission-log"` under the calling directory. The message just reports
#' the effective value so there is no surprise about where scripts and logs go.
#'
#' @return a message to display on attach
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' slurmtools_options_message()
#' }
slurmtools_options_message <- function() {
  root <- getOption('slurmtools.submission_root')
  root_line <- if (is.null(root)) {
    paste0(
      cli::col_cyan(cli::symbol$info),
      " slurmtools.submission_root: \"submission-log\" (default; set ",
      "options('slurmtools.submission_root') to change)"
    )
  } else {
    paste0(
      cli::col_green(cli::symbol$tick),
      " slurmtools.submission_root: ",
      root
    )
  }

  paste0(
    "\n\n",
    cli::rule(left = cli::style_bold("slurmtools")),
    "\n",
    root_line,
    "\n\n"
  )
}
