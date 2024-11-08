#' Cancels a running job
#'
#' @param job_id job id to cancel
#' @param confirm requires confirmation to cancel job
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @export
#'
#' @examples \dontrun{
#' cancel_job(243)
#' }
cancel_slurm_job <- function(job_id, confirm = TRUE) {

  current_user = if (is.null(Sys.getenv("USER"))) Sys.info()['user'] else Sys.getenv("USER")

  jobs <- get_slurm_jobs(user = current_user)

  if (!job_id %in% jobs$job_id) {
    if (job_id %in% get_slurm_jobs()$job_id) {
      message("This job is associated with a different user")
    }
    message(paste0("Below are the available jobs for you to cancel:"))
    print(get_slurm_jobs(user = current_user))
    stop(paste0("Please ensure the job id is associated your user_name: ", current_user))
  }

  job_id_filtered <- jobs %>% dplyr::filter(.data$job_id == .env$job_id)
  if (!job_id_filtered$job_state %in% c("RUNNING", "CONFIGURING")) {
    stop(paste0("Job: ", job_id, " is not running"))
  }

  if (confirm) {
    continue <- readline(
      paste0("You are about to cancel job: ", job_id, ". Are you sure you want to cancel? [Y/n]\n")
    )
  } else {
    continue <- "Y"
  }

  if (continue == "Y") {
    result <- processx::run("scancel", args = c(as.character(job_id)))
    if (result$status != 0) {
      print(paste0("Stdout: ", result$stdout))
      print(paste0("Stderr: ", result$stderr))
    }
  } else if (tolower(continue) == 'n') {
    stop("Job NOT cancelled.")
  } else {
    stop("You must enter Y or n")
  }
}
