utils::globalVariables(c("user_name"))

#' Function to parse each job into a tibble row
#'
#' @param job output from parse_jobs_json
#'
#' @return tibble of parsed jobs
#' @keywords internal
#' @noRd
parse_job_to_row <- function(job) {
  # check options for squeue version
  # alter parsing based on result

  if (
    package_version(getOption("squeue.version"))[1, 1:2] >
      package_version("23.02")
  ) {
    submit_time <- job$submit_time$number
    start_time <- job$start_time$number
    end_time <- job$end_time$number
    ### This is "hacky", it looks like for configuring model the list is 3 {"Running", "Configuring", "Power_up_node"}
    if (length(job$job_state) == 3) {
      job_state <- job$job_state[[2]]
    } else {
      job_state <- job$job_state[[1]]
    }
  } else {
    submit_time <- job$submit_time
    start_time <- job$start_time
    end_time <- job$end_time
    job_state <- job$job_state
  }

  tibble::tibble(
    job_id = job$job_id,
    job_state = job_state,
    job_name = job$name,
    cpus = job$cpus$number,
    partition = job$partition,
    standard_input = job$standard_input,
    standard_output = job$standard_output,
    submit_time = submit_time,
    start_time = start_time,
    end_time = end_time,
    user_name = job$user_name,
    current_working_directory = job$current_working_directory
  )
}

#' Function to transform squeue --json output into named list
#'
#' @param .json output from squeue --json call
#'
#' @return list of parsed output
#' @keywords internal
#' @noRd
parse_jobs_json <- function(.json) {
  if (!length(.json$jobs)) {
    empty <- tibble::tibble(
      job_id = "",
      job_state = "",
      job_name = "",
      cpus = "",
      partition = "",
      standard_input = "",
      standard_output = "",
      submit_time = "",
      start_time = "",
      end_time = "",
      user_name = "",
      current_working_directory = "",
    )
    return(empty)
  }
  purrr::list_rbind(purrr::map(.json$jobs, parse_job_to_row))
}

#' Gets the jobs run on slurm as a tibble
#'
#' @param user optional user name to filter jobs only submitted by user
#'
#' @return a tibble containing the jobs submitted to slurm
#' @export
#'
#' @examples \dontrun{
#' get_slurm_jobs()
#' }
get_slurm_jobs <- function(user = NULL) {
  cmd <- list(cmd = Sys.which("squeue"), args = "--json")
  res <- processx::run(cmd$cmd, args = cmd$args)
  if (res$status != 0) {
    # todo: better handle returning why
    rlang::abort(
      "unable to get slurm jobs, test what the output would be by running `squeue --json`"
    )
  }
  res_df <- parse_jobs_json(jsonlite::fromJSON(
    res$stdout,
    simplifyVector = FALSE
  ))
  if (all(sapply(res_df, function(column) all(is.na(column) | column == "")))) {
    return(res_df)
  }
  res_df$submit_time <- as.POSIXct(res_df$submit_time, origin = "1970-01-01")
  res_df$start_time <- as.POSIXct(res_df$start_time, origin = "1970-01-01")
  res_df$end_time <- as.POSIXct(res_df$end_time, origin = "1970-01-01")
  res_df <- res_df %>%
    dplyr::mutate(
      time = dplyr::case_when(
        job_state == "RUNNING" ~ round(Sys.time()) - round(start_time),
        job_state == "CONFIGURING" ~ round(Sys.time()) - round(submit_time),
        TRUE ~ round(end_time) - round(start_time)
      ) %>%
        hms::as_hms()
    )

  res_df <- res_df %>%
    dplyr::select(
      "job_id",
      "partition",
      "job_name",
      "user_name",
      "job_state",
      "time",
      "cpus",
      dplyr::everything()
    )

  if (!is.null(user)) {
    df <- tryCatch(
      {
        dplyr::filter(res_df, user_name == user)
      },
      error = function(e) {
        res_df
      }
    )
  } else {
    df <- res_df
  }

  return(df)
}
