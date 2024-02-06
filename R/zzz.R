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

  tryCatch(
    expr = {
      if (is.null(getOption('slurmtools.partitions'))) {
        rlang::warn("option('slurmtools.partitions') is not set")
      } # if
    }, # expr

    warning = {
      options('slurmtools.partitions' = get_slurm_partitions())
      message("option('slurmtools.partitions') was set automatically:")
      print((options('slurmtools.partitions')))
    } # warning

  ) # tryCatch

}
