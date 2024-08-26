utils::globalVariables(c("user_name"))

# Function to parse each job into a tibble row
parse_job_to_row <- function(job) {
  # check options for squeue version
  # alter parsing based on result
  tibble::tibble(
    job_id = job$job_id,
    job_state = job$job_state,
    cpus = job$cpus$number,
    partition = job$partition,
    standard_input = job$standard_input,
    standard_output = job$standard_output,
    submit_time = job$submit_time,
    start_time = job$start_time,
    user_name = job$user_name,
    current_working_directory = job$current_working_directory
  )
}

parse_jobs_json <- function(.json) {
  if (!length(.json$jobs)) {
    return(NULL)
  }
  purrr::list_rbind(purrr::map(.json$jobs, parse_job_to_row))
}

#' get slurm jobs
#' @param user user filter
#' @export
get_slurm_jobs <- function(user = NULL){
  cmd <- list(cmd = "squeue", args = "--json")
  res <- processx::run(cmd$cmd, args = cmd$args)
  if (res$status != 0) {
    # todo: better handle returning why
    rlang::abort("unable to get slurm jobs, test what the output would be by running `squeue --json`")
  }
  res_df <- parse_jobs_json(jsonlite::fromJSON(res$stdout, simplifyVector = FALSE))
  res_df$submit_time <- as.POSIXct(res_df$submit_time, origin = "1970-01-01")
  res_df$start_time <- as.POSIXct(res_df$start_time, origin = "1970-01-01")
  if (!is.null(user)) {
    return(dplyr::filter(res_df, user_name == user))
  }
  res_df
}
