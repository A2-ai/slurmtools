# the id(s) behind whatever names a job: the result of submit_slurm_job(), or
# job ids as given
slurm_job_id <- function(x) {
  if (is.list(x) && !is.null(x$job_id)) {
    return(as.character(x$job_id))
  }
  if ((is.character(x) || is.numeric(x)) && length(x) > 0 && !anyNA(x)) {
    return(as.character(x))
  }
  rlang::abort(
    "expected the result of submit_slurm_job(), or one or more job ids"
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
#' @param job the result of [submit_slurm_job()], or one or more job ids
#'
#' @return a tibble with one row per job `sacct` knows: `job_id`, `job_name`,
#'   `partition`, `state` (`PENDING`, `RUNNING`, `COMPLETED`, `FAILED`,
#'   `CANCELLED`, ...), `exit_code` (`"0:0"` is success), `elapsed`, `node`,
#'   and `submit`, `start`, `end` as date-times (`NA` until they happen). A
#'   job `sacct` does not know is simply absent.
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm_job(rscript, slurm_template_opts = list(file = "sim.R"))
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
