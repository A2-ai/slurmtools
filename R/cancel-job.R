#' Cancels a running job
#'
#' @param job_id job id to cancel, or the result of [submit_slurm_job()]
#' @param auto_confirm does not prompt user before cancelling job
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @export
#'
#' @examples \dontrun{
#' cancel_job(243)
#' }
cancel_slurm_job <- function(job_id, auto_confirm = FALSE) {
  job_id <- slurm_job_id(job_id)
  # an unset variable reads as "", never NULL, so the old is.null() test never fell back
  current_user <- Sys.getenv("USER")
  if (!nzchar(current_user)) {
    current_user <- Sys.info()[["user"]]
  }

  jobs <- get_slurm_jobs(user = current_user)

  if (!job_id %in% jobs$job_id) {
    if (job_id %in% get_slurm_jobs()$job_id) {
      message("This job is associated with a different user")
    }
    message(paste0("Below are the available jobs for you to cancel:"))
    print(jobs %>% filter(.data$user_name == current_user))
    stop(paste0(
      "Please ensure the job id is associated your user_name: ",
      current_user
    ))
  }

  # using .env$job_id due to argument name and jobs having the same
  # name. This allows for the comparison.
  job_id_filtered <- jobs %>% dplyr::filter(.data$job_id == .env$job_id)
  if (!job_id_filtered$job_state %in% c("RUNNING", "CONFIGURING", "PENDING")) {
    stop(paste0("Job: ", job_id, " is not running"))
  }

  if (!auto_confirm) {
    continue <- readline(
      paste0(
        "You are about to cancel job: ",
        job_id,
        ". Are you sure you want to cancel? [Y/n]\n"
      )
    )
    continue <- toupper(continue)
  } else {
    continue <- "Y"
  }

  if (continue == "Y") {
    run_slurm("scancel", as.character(job_id)) # a failure is raised with scancel's own message
    cli::cli_inform(c(v = "cancelled job {job_id}"))
  } else if (tolower(continue) == "n") {
    stop("Job NOT cancelled.")
  } else {
    stop("You must enter Y or n")
  }
}
