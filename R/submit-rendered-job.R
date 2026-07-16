#' Validate and resolve the partition for a job
#'
#' @param partition partition name, or the vector of available partitions
#'   (in which case the first one is selected)
#' @param ncpu number of cpus requested, checked against the partition
#'
#' @return the single, validated partition name
#' @keywords internal
#' @noRd
validate_partition <- function(partition, ncpu) {
  if (is.null(partition)) {
    rlang::abort("no partition selected")
  }
  partition <- match.arg(partition, choices = get_slurm_partitions())
  log4r::debug(.le$logger, paste0("partition set to: ", partition))

  check_slurm_partitions(ncpu, partition)
  partition
}

#' Render a slurm job script and submit it with sbatch
#'
#' This is the shared worker behind every [submit_slurm_job()] method: it
#' fills a whisker template with `template_list`, writes the rendered script
#' to `submission_root`, and submits it via `sbatch`. Everything
#' engine-specific (which command runs on the node) must already be resolved
#' into `template_list` by the calling method.
#'
#' @param template_list a flat named list of variables to render into the
#'   template
#' @param script_name file name (not path) of the job script to write, e.g.
#'   `"1001.sh"` or `"analysis.R.sh"`
#' @param slurm_job_template_path path to the whisker template for the job
#'   script
#' @param submission_root directory to track job submission scripts and output
#' @param render_dir directory to render the template within; defaults to the
#'   working directory
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... arguments to pass to processx::run
#'
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition; otherwise the result of
#'   [processx::run()]
#' @keywords internal
#' @noRd
submit_rendered_job <- function(
  template_list,
  script_name,
  slurm_job_template_path,
  submission_root = getOption("slurmtools.submission_root"),
  render_dir = NULL,
  dry_run = FALSE,
  ...
) {
  if (is.null(slurm_job_template_path)) {
    rlang::abort(
      "no slurm job template supplied; set options('slurmtools.slurm_job_template_path') or pass slurm_job_template_path"
    )
  }
  if (!fs::file_exists(slurm_job_template_path)) {
    rlang::abort(sprintf(
      "slurm job template path not valid: `%s`",
      slurm_job_template_path
    ))
  }
  if (is.null(submission_root)) {
    rlang::abort(
      "no submission root supplied; set options('slurmtools.submission_root') or pass submission_root"
    )
  }

  # Resolve both paths to absolutes now, while the working directory is still
  # the caller's. Rendering and submission each run inside withr::with_dir(),
  # so a relative path would otherwise be re-resolved against the wrong
  # directory. fs::path_abs is purely lexical, so submission_root need not
  # exist yet (it is created below).
  slurm_job_template_path <- fs::path_abs(slurm_job_template_path)
  submission_root <- fs::path_abs(submission_root)

  log4r::info(
    .le$logger,
    paste0(
      "filling slurm job template file with: \n\t",
      paste(
        sapply(names(template_list), function(name) {
          paste0(name, ": ", template_list[[name]])
        }),
        collapse = "\n\t"
      )
    )
  )

  render <- function() {
    tmpl <- brio::read_file(slurm_job_template_path)
    whisker::whisker.render(tmpl, template_list)
  }
  template_script <- if (is.null(render_dir)) {
    render()
  } else {
    withr::with_dir(render_dir, render())
  }

  script_file_path <- file.path(submission_root, script_name)
  if (!dry_run) {
    if (!fs::dir_exists(submission_root)) {
      log4r::info(.le$logger, "Creating submission root now")
      fs::dir_create(submission_root)
    }
    log4r::debug(.le$logger, "Writing script now")
    brio::write_file(template_script, script_file_path)
    fs::file_chmod(script_file_path, "0755")
  }

  cmd <- list(
    cmd = Sys.which("sbatch"),
    args = script_file_path,
    template_script = template_script,
    partition = template_list$partition
  )
  if (dry_run) {
    return(cmd)
  }
  log4r::info(.le$logger, "Submitting job now")
  withr::with_dir(submission_root, {
    processx::run(cmd$cmd, cmd$args, ...)
  })
}
