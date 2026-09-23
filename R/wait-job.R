# the states sacct reports once a job is over
terminal_job_states <- c(
  "COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "OUT_OF_MEMORY",
  "NODE_FAIL", "PREEMPTED", "BOOT_FAIL", "DEADLINE"
)

#' Wait for a job to finish
#'
#' @description
#' Looks at [slurm_job_status()] every `poll` seconds until the job reaches a
#' terminal state — `COMPLETED`, `FAILED`, `CANCELLED`, `TIMEOUT`,
#' `OUT_OF_MEMORY`, `NODE_FAIL`, `PREEMPTED`, `BOOT_FAIL` or `DEADLINE` — and
#' hands back that final status row. A job that failed is returned, not
#' thrown: read its `state` and `exit_code`. This is the R-side "post hook":
#' `wait_for_slurm_job(job)`, then ordinary R.
#'
#' A job `sacct` does not know yet shows as "not in sacct yet" and is polled
#' like any other, so a fresh submission is never mistaken for a missing job.
#'
#' @param job the [Job] returned by [submit_slurm_job()], or one job id
#' @param poll seconds between two looks at `sacct`
#' @param timeout seconds to wait in total before giving up with an error;
#'   `Inf` (the default) waits as long as it takes
#'
#' @return the job's final [slurm_job_status()] row, invisibly
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm_job(rscript, file = "sim.R", partition = "cpu2mem4gb")
#' status <- wait_for_slurm_job(job)
#' status$state # "COMPLETED"
#' slurm_job_log(job)
#' }
#' @export
wait_for_slurm_job <- function(job, poll = 10, timeout = Inf) {
  id <- slurm_job_id(job)
  if (length(id) != 1) {
    rlang::abort("wait_for_slurm_job() waits for one job; got several ids")
  }
  if (!is.numeric(poll) || length(poll) != 1 || is.na(poll) || poll <= 0) {
    rlang::abort("`poll` must be a positive number of seconds")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    rlang::abort("`timeout` must be a positive number of seconds, or Inf")
  }

  deadline <- Sys.time() + timeout
  name <- id
  state <- "not in sacct yet"
  cli::cli_progress_step("job {name}: {state}", spinner = TRUE)
  repeat {
    status <- slurm_job_status(id)
    if (nrow(status) > 0) {
      name <- sprintf("%s (%s)", status$job_name[[1]], id)
      state <- status$state[[1]]
    }
    cli::cli_progress_update()
    if (state %in% terminal_job_states) {
      break
    }
    if (Sys.time() >= deadline) {
      rlang::abort(c(
        sprintf("timed out after %s s waiting for job %s", format(timeout), id),
        i = sprintf("last state: %s", state)
      ))
    }
    Sys.sleep(poll)
  }
  invisible(status)
}
