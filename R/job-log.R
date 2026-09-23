#' Read a job's log
#'
#' @description
#' The lines of the job's standard output — or, with `which = "error"`, its
#' standard error — read from the path the [Job] handle recorded at submit
#' time, `%j` already resolved. Before the job has started there is no file,
#' and that is an error rather than an empty result, so "printed nothing" and
#' "has not run" cannot be confused.
#'
#' @param job the [Job] returned by [submit_slurm_job()]
#' @param which `"output"` (the default) or `"error"`
#' @param n keep only the last `n` lines; `Inf` (the default) keeps them all
#'
#' @return a character vector, one element per line
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm_job(rscript, file = "sim.R", partition = "cpu2mem4gb")
#' wait_for_slurm_job(job)
#' slurm_job_log(job)
#' slurm_job_log(job, "error", n = 20)
#' }
#' @export
slurm_job_log <- function(job, which = c("output", "error"), n = Inf) {
  if (!S7::S7_inherits(job, Job)) {
    rlang::abort(c(
      "`job` must be the Job returned by submit_slurm_job()",
      i = "only the handle knows where the logs went; for a bare id, read the path you gave `--output`"
    ))
  }
  which <- rlang::arg_match(which)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) {
    rlang::abort("`n` must be a non-negative number of lines, or Inf")
  }

  path <- if (which == "output") job@output else job@error
  if (is.na(path)) {
    rlang::abort(c(
      sprintf("this job's `--%s` was left to slurm, so the handle does not know where it went", which),
      i = sprintf("give the template with_%s(), or read slurm's default `slurm-%s.out` yourself", which, job@job_id)
    ))
  }
  if (!fs::file_exists(path)) {
    rlang::abort(c(
      sprintf("no %s log yet at `%s`", which, path),
      i = "the job may not have started: slurm_job_status(job)"
    ))
  }

  lines <- readLines(path, warn = FALSE)
  if (is.finite(n)) {
    lines <- utils::tail(lines, n)
  }
  lines
}
