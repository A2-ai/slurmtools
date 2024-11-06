#' Cancels a running job
#'
#' @param job_id job id to cancel
#' @param user optional if not the current user.
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @export
#'
#' @examples \dontrun{
#' cancel_job(243)
#' }
cancel_job <- function(job_id, user = NULL) {

  current_user = if (is.null(Sys.getenv("USER"))) Sys.info()['user'] else Sys.getenv("USER")

  if (!is.null(user) && user != current_user) {
    cat("The supplied user does not match the current user.\n")
    cat(paste0("\tsupplied user: ", user, "\n\tcurrent_user: ", current_user, "\n"))
    cat("You might be cancelling someone elses job.")
    continue <- readline("Are you sure you want to cancel this job? (Y/n)\n")
    if (continue == "Y") {
      current_user <- user
    } else if (tolower(continue) == "n") {
      stop(paste0("Not cancelling job: ", job_id))
    } else {
      stop("Please enter Y or n")
    }
  }

  jobs <- get_slurm_jobs(user = current_user)

  if (!job_id %in% jobs$job_id) {
    stop("Please ensure the job id is correct.")
  }

  job_id_filtered <- jobs %>% dplyr::filter(.data$job_id == .env$job_id)
  if (!job_id_filtered$job_state %in% c("RUNNING", "CONFIGURING")) {
    stop(paste0("Job: ", job_id, " is not running"))
  }

  result <- processx::run("scancel", args = c(as.character(job_id)))
  if (result$status != 0) {
    print(paste0("Stdout: ", result$stdout))
    print(paste0("Stderr: ", result$stderr))
  }
}
