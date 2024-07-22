.onLoad <- function(libname, pkgname) {
  if (is.null(getOption('slurmtools.slurm_job_template_path'))) {
    rlang::warn(
      "option('slurmtools.slurm_job_template_path') is not set. Please set it for job submission defaults to work."
    )
  }

  if (is.null(getOption('slurmtools.submission_root'))) {
    rlang::warn("option('slurmtools.submission_root') is not set. Please set it for  job submission defaults to work.")
  }

  if (is.null(getOption('slurmtools.bbi_config_path'))) {
    rlang::warn("option('slurmtools.bbi_config_path') is not set. Please set it for  job submission defaults to work.")
  }
}

if (is.null(getOption('slurmtools.fsmonitor_exe_path'))) {
  rlang::warn(
    "option('slurmtools.fsmonitor_exe_path') is not set. Please set it for job submission defaults to work."
  )
}

if (is.null(getOption('slurmtools.log_level'))) {
  rlang::warn(
    "option('slurmtools.log_level') is not set. Please set it for job submission defaults to work."
  )
}
