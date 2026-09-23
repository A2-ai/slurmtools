# The pre-template interface, kept so projects written against it keep working.
#
# submit_slurm_job() routes a call here when it looks like the NONMEM/bbi form
# (see is_legacy_submit()). The function below is `main`'s submit_slurm_job()
# at e4b747d, moved verbatim apart from its name, the `here` check (here moved
# to Suggests) and the roxygen block; do not "fix" it — its job is to behave
# exactly as it did.

#' main's submit_slurm_job(), verbatim
#'
#' @param .mod a path to a model or a bbi nonmem model object
#' @param partition name of the partition to submit the model
#' @param ncpu number of cpus to run the model against
#' @param overwrite whether to overwrite existing model results
#' @param dry_run return the command that would have been invoked, without invoking
#' @param ... arguments to pass to processx::run
#' @param slurm_job_template_path path to slurm job template
#' @param submission_root directory to track job submission scripts and output
#' @param bbi_config_path path to bbi.yaml file for bbi configuration
#' @param slurm_template_opts choose slurm template
#' @keywords internal
#' @noRd
submit_slurm_job_legacy <-
  function(
    .mod,
    partition = get_slurm_partitions(),
    ncpu = 1,
    overwrite = FALSE,
    dry_run = FALSE,
    ...,
    slurm_job_template_path = getOption("slurmtools.slurm_job_template_path"),
    submission_root = getOption("slurmtools.submission_root"),
    bbi_config_path = getOption("slurmtools.bbi_config_path"),
    slurm_template_opts = list()
  ) {
    log4r::debug(
      .le$logger,
      paste0(
        "Starting submit_slurm_job for .mod:\n\t",
        paste(
          sapply(names(.mod), function(name) {
            paste0(name, ": ", .mod[[name]])
          }),
          collapse = "\n\t"
        )
      )
    )

    if (is.null(partition)) {
      rlang::abort("no partition selected")
    }
    partition <- match.arg(partition)
    log4r::debug(.le$logger, paste0("partition set to: ", partition))

    check_slurm_partitions(ncpu, partition)

    if (
      !inherits(.mod, "bbi_nonmem_model") &&
        !fs::file_exists(.mod)
    ) {
      stop(
        "please provide a bbi_nonmem_model created via read_model/new_model, or a path to the model file"
      )
    }
    if (!inherits(.mod, "bbi_nonmem_model")) {
      # its a file path that exists so lets convert that into the structure bbi
      # provides for now, stripping extension
      .mod <- list(
        absolute_model_path = fs::path_abs(
          tools::file_path_sans_ext(.mod)
        )
      )
      log4r::debug(
        .le$logger,
        paste0("converted mod to bbi structrue: ", paste(.mod, collapse = ","))
      )
    }
    parallel <- if (ncpu > 1) {
      TRUE
    } else {
      FALSE
    }

    if (!fs::file_exists(slurm_job_template_path)) {
      rlang::abort(sprintf(
        "slurm job template path not valid: `%s`",
        slurm_job_template_path
      ))
    }
    if (overwrite && fs::dir_exists(.mod$absolute_model_path)) {
      log4r::info(
        .le$logger,
        paste0("Deleting existing directory: ", .mod$absolute_model_path)
      )
      fs::dir_delete(.mod$absolute_model_path)
    }

    if (is.null(slurm_template_opts$bbi_exe_path)) {
      bbi_exe_path <- Sys.which("bbi")
    } else {
      bbi_exe_path <- slurm_template_opts$bbi_exe_path
    }
    log4r::debug(.le$logger, paste0("bbi_exe_path set to: ", bbi_exe_path))

    if (is.null(slurm_template_opts$project_path) || is.null(slurm_template_opts$project_name)) {
      rlang::check_installed("here", "to name the project in a pre-template submit_slurm_job() call")
    }
    if (is.null(slurm_template_opts$project_path)) {
      project_path <- here::here()
    } else {
      project_path <- slurm_template_opts$project_path
    }
    log4r::debug(.le$logger, paste0("project_path set to: ", project_path))

    if (is.null(slurm_template_opts$project_name)) {
      project_name <- here::here() %>% basename()
    } else {
      project_name <- slurm_template_opts$project_name
    }
    log4r::debug(.le$logger, paste0("project_name set to: ", project_name))

    default_template_list <- list(
      partition = partition,
      parallel = parallel,
      ncpu = ncpu,
      job_name = sprintf("%s-nonmem-run", basename(.mod$absolute_model_path)),
      project_path = project_path,
      project_name = project_name,
      bbi_exe_path = bbi_exe_path,
      bbi_config_path = bbi_config_path,
      model_path = .mod$absolute_model_path
    )

    template_list <- c(
      default_template_list,
      slurm_template_opts
    )
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

    template_script <-
      withr::with_dir(dirname(.mod$absolute_model_path), {
        tmpl <- brio::read_file(slurm_job_template_path)
        whisker::whisker.render(
          tmpl,
          template_list
        )
      })

    script_file_path <-
      file.path(
        submission_root,
        sprintf(
          "%s.sh",
          basename(.mod$absolute_model_path)
        )
      )
    if (!dry_run) {
      if (!fs::dir_exists(submission_root)) {
        log4r::info(.le$logger, "Creating submission root now")
        fs::dir_create(submission_root)
      }
      log4r::debug(.le$logger, "Writing script now")
      brio::write_file(template_script, script_file_path)
      fs::file_chmod(script_file_path, "0755")
    }
    log4r::info(.le$logger, "Submitting job now")

    cmd <- list(
      cmd = Sys.which("sbatch"),
      args = script_file_path,
      template_script = template_script,
      partition = partition
    )
    if (dry_run) {
      return(cmd)
    }
    withr::with_dir(submission_root, {
      processx::run(cmd$cmd, cmd$args, ...)
    })
  }

# argument names only the pre-template interface had
legacy_arg_names <- c(".mod", "overwrite", "slurm_job_template_path", "bbi_config_path", "slurm_template_opts")

# does a submit_slurm_job() call look like the pre-template NONMEM/bbi form?
# `x` is NULL when the call named `.mod` instead; `dot_names` is ...names()
is_legacy_submit <- function(x, template_missing, dot_names) {
  any(dot_names %in% legacy_arg_names) ||
    inherits(x, "bbi_nonmem_model") ||
    (template_missing &&
      !S7::S7_inherits(x, Template) &&
      !is.null(getOption("slurmtools.slurm_job_template_path")))
}

warn_legacy_submit <- function() {
  rlang::warn(
    c(
      "`submit_slurm_job()` was called the pre-template way (a model plus `slurmtools.slurm_job_template_path`); running it as before",
      i = "the template way: submit_slurm_job(default_template() |> with_command(\"bbi\", ...), file = \"model.mod\")"
    ),
    .frequency = "once",
    .frequency_id = "slurmtools-legacy-submit"
  )
}
