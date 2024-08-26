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
  set_header <- cli::rule(
    left = cli::style_bold("Set slurmtools options")
  )

  unset_header <- cli::rule(
    left = cli::style_bold("Needed slurmtools options")
  )

  set_options <- c()
  unset_options <- c()
  print_set <- FALSE
  print_unset <- FALSE

  tmpl_path <- getOption('slurmtools.slurm_job_template_path')
  if (is.null(tmpl_path)) {
    unset_options <- c(unset_options, "option('slurmtools.slurm_job_template_path') is not set.")
    print_unset <- TRUE
  } else {
    set_options <- c(set_options, paste("slurmtools.slurm_jon_template_path:", tmpl_path))
    print_set <- TRUE
  }

  root <- getOption('slurmtools.submission_root')
  if (is.null(root)) {
    unset_options <- c(unset_options, "option('slurmtools.submission_root') is not set.")
    print_unset <- TRUE
  } else {
    set_options <- c(set_options, paste("slurmtools.submission_root:", root))
    print_set <- TRUE
  }

  set_bullets <- paste0(
    cli::col_green(cli::symbol$tick), " ", set_options, collapse = "\n"
  )

  unset_bullets <- paste0(
    cli::col_red(cli::symbol$cross), " ", unset_options, collapse = "\n"
  )

  unset <- paste0(
    cli::col_cyan(cli::symbol$info), " ",
    cli::format_inline("Please set all options for job submission defaults to work.")
  )
  if (print_set) {
    if (print_unset) {
      paste0(
        set_header, "\n",
        set_bullets, "\n",
        unset_header, "\n",
        unset_bullets, "\n",
        unset
      )
    } else {
      paste0(
        set_header, "\n",
        set_bullets, "\n"
      )
    }
  } else {
    paste0(
      unset_header, "\n",
      unset_bullets, "\n",
      unset
    )
  }
}

