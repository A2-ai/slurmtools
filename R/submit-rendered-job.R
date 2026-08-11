#' Render a slurm job script and submit it with sbatch
#'
#' This is the worker behind [submit_slurm_job()]. It fills a whisker template
#' with `template_list`, writes the rendered script to `submission_root`, and
#' submits it via `sbatch`. It knows nothing about what runs on the node: the
#' command lives in the template, and every value it needs (including any
#' paths) is already resolved into `template_list` by the caller. Paths flow
#' through untouched, the template owns any path handling.
#'
#' @param template_list a flat named list of variables to render into the
#'   template. Must include `partition` and `ncpu` for the partition check.
#' @param script_name file name (not path) of the job script to write, e.g.
#'   `"1001.sh"` or `"analysis.R.sh"`
#' @param template path to the whisker template for the job script
#' @param submission_root directory to track job submission scripts and output
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... arguments to pass to [processx::run()]
#'
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition; otherwise the result of
#'   [processx::run()]
#' @keywords internal
#' @noRd
submit_rendered_job <- function(
  template_list,
  script_name,
  template,
  submission_root = getOption("slurmtools.submission_root"),
  dry_run = FALSE,
  ...
) {
  if (is.null(template)) {
    rlang::abort("no slurm job template supplied")
  }
  if (!fs::file_exists(template)) {
    rlang::abort(sprintf("slurm job template not found: `%s`", template))
  }
  if (is.null(submission_root)) {
    rlang::abort(
      "no submission root supplied; set options('slurmtools.submission_root') or pass submission_root"
    )
  }

  log4r::info(
    .le$logger,
    paste0(
      "filling slurm job template with: \n\t",
      paste(
        sapply(names(template_list), function(name) {
          paste0(name, ": ", template_list[[name]])
        }),
        collapse = "\n\t"
      )
    )
  )

  rendered <- whisker::whisker.render(brio::read_file(template), template_list)
  script_file_path <- file.path(submission_root, script_name)

  cmd <- list(
    cmd = Sys.which("sbatch"),
    args = script_file_path,
    template_script = rendered,
    partition = template_list$partition
  )
  if (dry_run) {
    return(cmd)
  }

  if (!fs::dir_exists(submission_root)) {
    log4r::info(.le$logger, "Creating submission root now")
    fs::dir_create(submission_root)
  }
  log4r::debug(.le$logger, "Writing script now")
  brio::write_file(rendered, script_file_path)
  fs::file_chmod(script_file_path, "0755")

  log4r::info(.le$logger, "Submitting job now")
  processx::run(cmd$cmd, cmd$args, ...)
}
