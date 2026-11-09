# The Slurm binaries the new code runs (sinfo, sacct, squeue, scancel, sbatch)
# go through here, so a command that is missing or that fails is reported the
# same way everywhere, in the command's own words (deep-review finding 8).
# processx::run()'s default error, "System command 'sbatch' failed", hid them:
# sbatch's "Invalid directive found in batch script: model" never reached the
# user. Two calls stay as they were: the `squeue --version` probe in .onLoad(),
# and the pre-template NONMEM code in R/legacy.R, kept verbatim.
#
# `...` goes to processx::run() as it always did. error_on_status = FALSE
# there still means "give me the result, whatever the exit status".
run_slurm <- function(bin, args = character(), ...) {
  path <- Sys.which(bin)
  if (!nzchar(path)) {
    rlang::abort(
      c(
        sprintf("could not find %s on the PATH", bin),
        i = "slurmtools runs Slurm's own commands, so it needs the cluster's login node"
      ),
      class = "slurmtools_command_error"
    )
  }
  dots <- rlang::list2(...)
  check_status <- !isFALSE(dots$error_on_status)
  dots$error_on_status <- FALSE
  res <- rlang::exec(processx::run, unname(path), args, !!!dots)
  if (check_status && res$status != 0) {
    said <- trimws(res$stderr)
    rlang::abort(
      c(
        sprintf("%s failed (exit status %s)", bin, res$status),
        if (nzchar(said)) c(x = said)
      ),
      class = "slurmtools_command_error",
      status = res$status,
      stdout = res$stdout,
      stderr = res$stderr
    )
  }
  res
}
