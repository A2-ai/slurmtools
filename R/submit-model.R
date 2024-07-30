#' submit a nonmem model to slurm in parallel
#'
#' @param .mod a path to a model or a bbi nonmem model object
#' @param partition name of the partition to submit the model
#' @param ncpu number of cpus to run the model against
#' @param overwrite whether to overwrite existing model results
#' @param dry_run return the command that would have been invoked, without invoking
#' @param ... arguments to pass to processx::run
#' @param slurm_job_template_path path to slurm job template
#' @param submission_root directory to track job submission scripts and output
#' @param bbi_config_path path to bbi config file
#' @param log_level logging level for nmt watcher
#' @param fsmonitor_exe_path path to fsmonitor executable
#' @param slurm_template_opts choose slurm template
#'
#' @export
submit_nonmem_model <-
  function(.mod,
           partition = get_slurm_partitions(),
           ncpu = 1,
           overwrite = FALSE,
           dry_run = FALSE,
           ...,
           email = NULL,
           slurm_job_template_path = getOption('slurmtools.slurm_job_template_path'),
           submission_root = getOption('slurmtools.submission_root'),
           log_level = getOption('slurmtools.log_level'),
           alert_method = getOption('slurmtools.alert_method'),
           slurm_template_opts = list()) {

    if (is.null(partition)) {
      rlang::abort("no partition selected")
    }
    partition <- match.arg(partition)

    check_slurm_partitions(ncpu, partition)

    if (!inherits(.mod, "bbi_nonmem_model") &&
        !fs::file_exists(.mod)) {
      stop(
        "please provide a bbi_nonmem_model created via read_model/new_model, or a path to the model file"
      )
    }
    if (!inherits(.mod, "bbi_nonmem_model")) {
      # its a file path that exists so lets convert that into the structure bbi
      # provides for now
      .mod <- list(absolute_model_path = fs::path_abs(.mod))
    }
    parallel <- if (ncpu > 1) {
      TRUE
    } else {
      FALSE
    }

    if (!fs::file_exists(slurm_job_template_path)) {
      rlang::abort(sprintf("slurm job template path not valid: `%s`", slurm_job_template_path))
    }
    if (overwrite && fs::dir_exists(.mod$absolute_model_path)) {
      fs::dir_delete(.mod$absolute_model_path)
    }

    config_toml_path <- paste0(.mod$absolute_model_path, ".toml")
    if (!fs::file_exists(config_toml_path)) {
      generate_default_watcher_config(.mod)
    }

    config <- configr::read.config(config_toml_path)
    config$output_dir <- file.path(here::here(), config$output_dir)
    config$watched_dir <-file.path(here::here(), config$watched_dir)

    new_config_toml_path <-
      file.path(
        here::here(), "model", "nonmem", "submission-log",
        paste0(config$model_number, ".toml")
      )

    write_file(rextendr::to_toml(config), new_config_toml_path)

    template_script <-
      withr::with_dir(dirname(.mod$absolute_model_path), {
        tmpl <- brio::read_file(slurm_job_template_path)
        whisker::whisker.render(
          tmpl,
          list(
            partition = partition,
            parallel = parallel,
            ncpu = ncpu,
            job_name = sprintf("nonmem-run-%s", basename(.mod$absolute_model_path)),
            model_path = .mod$absolute_model_path,
            config_toml_path = new_config_toml_path,
            nmm_exe_path = Sys.which("nmm"),
            log_level = log_level,
            alert_method = alert_method,
            email = email
          )
        )
      })
    script_file_path <-
        file.path(submission_root, sprintf("%s.sh", basename(.mod$absolute_model_path)))
    if (!dry_run) {
      if (!fs::dir_exists(submission_root)) {
        fs::dir_create(submission_root)
      }
      brio::write_file(template_script, script_file_path)
      fs::file_chmod(script_file_path, "0755")
    }
    cmd <- list(cmd = "sbatch", args = script_file_path, template_script = template_script, partition = partition)
    if (dry_run) {
      return(cmd)
    }
    withr::with_dir(submission_root, {
      processx::run(cmd$cmd, cmd$args, ...)
    })
  }
