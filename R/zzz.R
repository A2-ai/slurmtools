.onAttach <- function(libname, pkgname) {
  if (is.null(getOption('slurmtools.slurm_job_template_path'))) {
    # rlang::warn(
    #   "option('slurmtools.slurm_job_template_path') is not set. Please set it for job submission defaults to work."
    # )
    packageStartupMessage("option('slurmtools.slurm_job_template_path') is not set. Please set it for job submission defaults to work.")
  }

  if (is.null(getOption('slurmtools.submission_root'))) {
    #rlang::warn("option('slurmtools.submission_root') is not set. Please set it for  job submission defaults to work.")
    packageStartupMessage("option('slurmtools.submission_root') is not set. Please set it for  job submission defaults to work.")
  }
}
