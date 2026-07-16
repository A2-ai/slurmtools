#' Submit a job to slurm
#'
#' @description
#' `submit_slurm_job()` submits work to slurm. It is an S3 generic: what runs
#' on the compute node is decided by what you pass as `.mod`, and each method
#' only exposes the inputs relevant to that kind of job.
#'
#' * a **bbr/bbi model** (`bbi_nonmem_model`) runs NONMEM via
#'   `bbi nonmem run local`, rendered into the slurm job template at
#'   `options('slurmtools.slurm_job_template_path')`.
#' * a **hyperion model** (`hyperion_nonmem_model`) is handed to
#'   `hyperion::submit_model_to_slurm()`, which carries its own job template
#'   and discovers the pharos config itself, so no slurmtools options are
#'   involved.
#' * a **path to a script** submits a general (non-NONMEM) job based on the
#'   file extension: `.R` runs `Rscript <path>`, `.qmd` runs
#'   `quarto render <path>`. The command is rendered into a general job
#'   template shipped with slurmtools. A bare path is *not* treated as a
#'   NONMEM model; pass `.mod`/`.ctl` files through `bbr::read_model()` or
#'   `hyperion::read_model()` instead.
#' * **any other file type** (say, a python script) can be submitted by
#'   supplying your own `slurm_job_template_path` that lays out the tool
#'   call, plus whatever variables it needs via `slurm_template_opts`.
#'   With a user-supplied template, `submit_slurm_job()`'s job drops to
#'   filling in the template and calling `sbatch` on it. In addition to
#'   your `slurm_template_opts`, the template can use `{{partition}}`,
#'   `{{ncpu}}`, `{{parallel}}`, `{{job_name}}`, `{{script_path}}`,
#'   `{{workdir}}`, `{{project_path}}`, `{{project_name}}`, and
#'   `{{command}}` (built for `.R`/`.qmd`, or passed as
#'   `slurm_template_opts$command`).
#'
#' @param .mod what to submit: a `bbi_nonmem_model`, a
#'   `hyperion_nonmem_model`, or a path to a `.R`/`.qmd` file
#' @param partition name of the partition to submit the job to
#' @param ncpu number of cpus to run the job against
#' @param overwrite whether to overwrite existing model results
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... additional arguments passed on to the method; the bbi and
#'   character methods forward them to [processx::run()], the hyperion method
#'   forwards them to `hyperion::submit_model_to_slurm()`
#' @param slurm_job_template_path path to slurm job template. The bbi method
#'   defaults to `options('slurmtools.slurm_job_template_path')`; the
#'   character method defaults to the general template shipped with slurmtools
#' @param submission_root directory to track job submission scripts and output
#' @param bbi_config_path path to bbi.yaml file for bbi configuration
#' @param slurm_template_opts named list of extra variables to render into the
#'   job template (and overrides for `bbi_exe_path`, `project_path`,
#'   `project_name`)
#'
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition; otherwise the result of submitting
#'   the job
#'
#' @examples
#' \dontrun{
#' # a bbr model
#' mod <- bbr::read_model("model/nonmem/1001")
#' submit_slurm_job(mod, partition = "cpu2mem4gb", ncpu = 2)
#'
#' # a hyperion model
#' mod <- hyperion::read_model("model/nonmem/1001.ctl")
#' submit_slurm_job(mod, partition = "cpu2mem4gb", ncpu = 2)
#'
#' # a plain R script or quarto document
#' submit_slurm_job("scripts/big-simulation.R", ncpu = 4)
#' submit_slurm_job("reports/analysis.qmd")
#'
#' # anything else: bring your own template laying out the tool call
#' submit_slurm_job(
#'   "scripts/train.py",
#'   slurm_job_template_path = "slurm-python.tmpl",
#'   slurm_template_opts = list(conda_env = "ml")
#' )
#' }
#' @export
submit_slurm_job <- function(.mod, ...) {
  UseMethod("submit_slurm_job")
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.bbi_nonmem_model <- function(
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

  partition <- validate_partition(partition, ncpu)
  model_path <- .mod$absolute_model_path

  if (overwrite && fs::dir_exists(model_path)) {
    log4r::info(
      .le$logger,
      paste0("Deleting existing directory: ", model_path)
    )
    fs::dir_delete(model_path)
  }

  parallel <- ncpu > 1

  if (is.null(slurm_template_opts$bbi_exe_path)) {
    bbi_exe_path <- Sys.which("bbi")
  } else {
    bbi_exe_path <- slurm_template_opts$bbi_exe_path
  }
  log4r::debug(.le$logger, paste0("bbi_exe_path set to: ", bbi_exe_path))

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

  command <- if (parallel) {
    sprintf(
      "%s nonmem run local %s.mod --parallel --threads=%s --config %s",
      bbi_exe_path,
      model_path,
      ncpu,
      bbi_config_path
    )
  } else {
    sprintf(
      "%s nonmem run local %s.mod --config %s",
      bbi_exe_path,
      model_path,
      bbi_config_path
    )
  }

  default_template_list <- list(
    partition = partition,
    parallel = parallel,
    ncpu = ncpu,
    job_name = sprintf("%s-nonmem-run", basename(model_path)),
    project_path = project_path,
    project_name = project_name,
    bbi_exe_path = bbi_exe_path,
    bbi_config_path = bbi_config_path,
    model_path = model_path,
    command = command,
    workdir = dirname(model_path)
  )

  template_list <- c(
    default_template_list,
    slurm_template_opts
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", basename(model_path)),
    slurm_job_template_path = slurm_job_template_path,
    submission_root = submission_root,
    render_dir = dirname(model_path),
    dry_run = dry_run,
    ...
  )
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.hyperion_nonmem_model <- function(
  .mod,
  partition = get_slurm_partitions(),
  ncpu = 1,
  overwrite = FALSE,
  dry_run = FALSE,
  ...
) {
  if (!requireNamespace("hyperion", quietly = TRUE)) {
    rlang::abort(
      "the hyperion package must be installed to submit hyperion models"
    )
  }
  partition <- validate_partition(partition, ncpu)

  log4r::debug(
    .le$logger,
    paste0(
      "delegating hyperion model submission to hyperion::submit_model_to_slurm"
    )
  )

  # hyperion carries its own job template and discovers the pharos config
  # itself, so no slurmtools template/config options apply here
  hyperion::submit_model_to_slurm(
    .mod,
    overwrite = overwrite,
    dry_run = dry_run,
    ncpu = ncpu,
    partition = partition,
    ...
  )
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.character <- function(
  .mod,
  partition = get_slurm_partitions(),
  ncpu = 1,
  dry_run = FALSE,
  ...,
  slurm_job_template_path = NULL,
  submission_root = getOption("slurmtools.submission_root"),
  slurm_template_opts = list()
) {
  if (length(.mod) != 1) {
    rlang::abort("`.mod` must be a single file path")
  }
  if (!fs::file_exists(.mod)) {
    rlang::abort(sprintf("no such file: `%s`", .mod))
  }

  script_path <- fs::path_abs(.mod)
  ext <- tolower(fs::path_ext(script_path))

  # a user-supplied template is the power-user contract: they lay out the
  # tool call and any variables themselves (via slurm_template_opts), and
  # submit_slurm_job only fills in the template and calls sbatch on it
  custom_template <- !is.null(slurm_job_template_path)

  command <- slurm_template_opts$command
  if (is.null(command)) {
    command <- switch(
      ext,
      r = sprintf("%s %s", Sys.which("Rscript"), script_path),
      qmd = sprintf("%s render %s", Sys.which("quarto"), script_path),
      mod = ,
      ctl = if (custom_template) {
        NULL
      } else {
        rlang::abort(
          c(
            sprintf(
              "a bare path is not treated as a NONMEM model: `%s`",
              .mod
            ),
            i = "read it first, then submit the model object:",
            i = "bbr::read_model() or hyperion::read_model()"
          )
        )
      },
      if (custom_template) {
        NULL
      } else {
        rlang::abort(
          c(
            sprintf(
              "don't know how to submit a `.%s` file to slurm (supported: .R, .qmd)",
              ext
            ),
            i = "for other file types, supply your own slurm_job_template_path laying out the tool call"
          )
        )
      }
    )
  }
  log4r::debug(
    .le$logger,
    paste0(
      "command set to: ",
      if (is.null(command)) "<laid out in template>" else command
    )
  )

  partition <- validate_partition(partition, ncpu)

  if (!custom_template) {
    slurm_job_template_path <- system.file(
      "templates",
      "slurm-job-generic.tmpl",
      package = "slurmtools"
    )
  }

  if (is.null(slurm_template_opts$project_path)) {
    project_path <- here::here()
  } else {
    project_path <- slurm_template_opts$project_path
  }

  if (is.null(slurm_template_opts$project_name)) {
    project_name <- here::here() %>% basename()
  } else {
    project_name <- slurm_template_opts$project_name
  }

  default_template_list <- list(
    partition = partition,
    parallel = ncpu > 1,
    ncpu = ncpu,
    job_name = basename(script_path),
    project_path = project_path,
    project_name = project_name,
    script_path = script_path,
    workdir = dirname(script_path)
  )
  default_template_list$command <- command

  template_list <- c(
    default_template_list,
    slurm_template_opts
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", basename(script_path)),
    slurm_job_template_path = slurm_job_template_path,
    submission_root = submission_root,
    render_dir = dirname(script_path),
    dry_run = dry_run,
    ...
  )
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.default <- function(.mod, ...) {
  rlang::abort(
    c(
      sprintf(
        "don't know how to submit an object of class <%s> to slurm",
        paste(class(.mod), collapse = "/")
      ),
      i = "supply a bbr model, a hyperion model, or a path to a .R/.qmd file"
    )
  )
}
