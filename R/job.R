validate_job_id <- function(value) {
  if (length(value) != 1 || is.na(value) || !grepl("^[0-9]+(_[0-9]+)?$", value)) {
    "must be a slurm job id, like \"2049\""
  }
}

#' A submitted slurm job
#'
#' @description
#' What [submit_slurm_job()] returns for a [Template()]: the job id sbatch
#' handed back, plus where the job's script and logs are, with `%j` already
#' resolved to the id. Print it to see them; the raw `sbatch` result is kept
#' under `@sbatch`. Hand it to [slurm_job_status()] or [cancel_slurm_job()].
#'
#' @param job_id the id sbatch returned, as a string
#' @param job_name the job's `--job-name`
#' @param partition the partition requested, or `NULL` for the cluster default
#' @param submit_time when sbatch accepted the job
#' @param script path of the rendered job script
#' @param output,error paths of the job's stdout and stderr logs, `%j`
#'   resolved; `NA` when the template routes them elsewhere without a value
#' @param sbatch the [processx::run()] result of the sbatch call
#'
#' @export
Job <- S7::new_class(
  "Job",
  package = "slurmtools",
  properties = list(
    job_id = S7::new_property(S7::class_character, validator = validate_job_id),
    job_name = S7::class_character,
    partition = S7::new_union(S7::class_character, NULL),
    submit_time = S7::new_property(S7::class_POSIXct, default = quote(Sys.time())),
    script = S7::class_character,
    output = S7::class_character,
    error = S7::class_character,
    sbatch = S7::class_list
  )
)

#' @export
S7::method(print, Job) <- function(x, ...) {
  partition <- if (is.null(x@partition)) "default partition" else x@partition
  cat(sprintf(
    "<Job> %s \u00b7 %s \u00b7 %s \u00b7 submitted %s\n",
    x@job_id,
    x@job_name,
    partition,
    format(x@submit_time, "%Y-%m-%d %H:%M:%S")
  ))
  cat(sprintf("  script  %s\n  output  %s\n  error   %s\n", x@script, x@output, x@error))
  cat("\u2139 wait_for_slurm_job(job) \u00b7 slurm_job_status(job) \u00b7 slurm_job_log(job) \u00b7 cancel_slurm_job(job)\n")
  invisible(x)
}

# the id(s) behind whatever names a job: a Job handle, or job ids as given
slurm_job_id <- function(x) {
  if (S7::S7_inherits(x, Job)) {
    return(x@job_id)
  }
  if ((is.character(x) || is.numeric(x)) && length(x) > 0 && !anyNA(x)) {
    return(as.character(x))
  }
  rlang::abort(
    "expected the Job returned by submit_slurm_job(), or one or more job ids"
  )
}

#' Status of submitted jobs
#'
#' @description
#' Asks `sacct` about one job or several and returns one row per job.
#' `sacct` still knows a job after it has finished, where `squeue` (behind
#' [get_slurm_jobs()]) forgets it a few minutes later, so this is the call
#' for "did it work?" as well as "is it running?".
#'
#' @param job the [Job] returned by [submit_slurm_job()], or one or more job
#'   ids
#'
#' @return a tibble with one row per job `sacct` knows: `job_id`, `job_name`,
#'   `partition`, `state` (`PENDING`, `RUNNING`, `COMPLETED`, `FAILED`,
#'   `CANCELLED`, ...), `exit_code` (`"0:0"` is success), `elapsed`, `node`,
#'   and `submit`, `start`, `end` as date-times (`NA` until they happen). A
#'   job `sacct` does not know is simply absent.
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm_job(rscript, file = "sim.R", partition = "cpu2mem4gb")
#' slurm_job_status(job)
#' slurm_job_status(c(2051, 2052))
#' }
#' @export
slurm_job_status <- function(job) {
  ids <- slurm_job_id(job)
  sacct <- Sys.which("sacct")
  if (!nzchar(sacct)) {
    rlang::abort("could not find sacct binary")
  }
  cols <- c(
    "job_id", "job_name", "partition", "state", "exit_code", "elapsed", "node",
    "submit", "start", "end"
  )
  res <- processx::run(sacct, c(
    "-j", paste(ids, collapse = ","),
    "--allocations", "--parsable2", "--noheader",
    "--format=JobID,JobName,Partition,State,ExitCode,Elapsed,NodeList,Submit,Start,End"
  ))

  if (!nzchar(trimws(res$stdout))) {
    df <- as.data.frame(
      stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols)
    )
  } else {
    df <- utils::read.table(
      text = res$stdout,
      sep = "|",
      quote = "",
      comment.char = "",
      colClasses = "character",
      col.names = cols,
      fill = TRUE
    )
  }
  df$state <- sub(" by .*$", "", df$state) # "CANCELLED by 603603109"
  for (col in c("submit", "start", "end")) {
    df[[col]] <- as.POSIXct(df[[col]], format = "%Y-%m-%dT%H:%M:%S")
  }
  tibble::as_tibble(df)
}
