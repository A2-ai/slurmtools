#' Submit a job to slurm
#'
#' @description
#' `submit_slurm_job()` submits work to slurm. It is an S3 generic: what runs on
#' the compute node is decided by what you pass as `input`, and the command
#' itself lives in the job **template**, not in this function. Each method's only
#' job is to assemble the variables a template needs and hand them to the shared
#' renderer.
#'
#' * a **bbr/bbi model** (`bbi_nonmem_model`) renders the shipped
#'   `nonmem-bbi` template (a `bbi nonmem run local` command).
#' * a **hyperion model** (`hyperion_nonmem_model`) is handed to
#'   `hyperion::submit_model_to_slurm()`, which carries its own template and
#'   discovers its own pharos config.
#' * a **path to a script** renders a shipped template by file extension:
#'   `.R` uses `rscript`, `.qmd` uses `quarto`. A character vector of paths
#'   submits one job per path. A bare `.mod`/`.ctl` path is refused — read it
#'   into a model object first.
#' * **any other file type** can be submitted by supplying your own `template`
#'   plus whatever variables it needs via `template_opts`.
#'
#' Shipped templates live in `system.file("templates", package = "slurmtools")`
#' and are based on the pharos slurm template. Besides your own `template_opts`,
#' a template can use `{{job_name}}`, `{{partition}}`, `{{ncpu}}`,
#' `{{parallel}}`, `{{num_mpi_cpus}}`, `{{account}}`, `{{log_path}}`, and
#' (per method) `{{model_path}}`, `{{config_path}}`, or `{{script_path}}`.
#'
#' @param input what to submit: a `bbi_nonmem_model`, a `hyperion_nonmem_model`,
#'   or a path (or vector of paths) to a `.R`/`.qmd` file
#' @param partition name of the partition to submit to. The default passes the
#'   full set of partitions; the first is selected.
#' @param ncpu number of cpus to request
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param submission_root directory to track job submission scripts and output
#' @param template path to a whisker job template. `NULL` selects the shipped
#'   template for the input type; supplying your own is the power-user contract.
#' @param template_opts named list of extra variables to render into the
#'   template (these override the values a method builds)
#' @param ... additional arguments passed on to the method; the bbi and
#'   character methods forward them to [processx::run()], the hyperion method
#'   forwards them to `hyperion::submit_model_to_slurm()`
#'
#' @return for a dry run, a list with the sbatch command, its args, the rendered
#'   script, and the partition; otherwise the result of submitting the job
#'
#' @examples
#' \dontrun{
#' # a bbr model
#' mod <- bbr::read_model("model/nonmem/1001")
#' submit_slurm_job(mod, partition = "cpu2mem4gb", ncpu = 2, config_path = "bbi.yaml")
#'
#' # a hyperion model
#' mod <- hyperion::read_model("model/nonmem/1001.ctl")
#' submit_slurm_job(mod, partition = "cpu2mem4gb", ncpu = 2)
#'
#' # scripts (one or many)
#' submit_slurm_job("scripts/big-simulation.R", ncpu = 4)
#' submit_slurm_job(c("a.R", "b.R", "report.qmd"))
#'
#' # anything else: bring your own template laying out the command
#' submit_slurm_job(
#'   "scripts/train.py",
#'   template = "slurm-python.tmpl",
#'   template_opts = list(conda_env = "ml")
#' )
#' }
#' @export
submit_slurm_job <- function(
  input,
  partition = get_slurm_partitions(),
  ncpu = 1,
  dry_run = FALSE,
  submission_root = getOption("slurmtools.submission_root"),
  template = NULL,
  template_opts = list(),
  ...
) {
  UseMethod("submit_slurm_job")
}

#' @rdname submit_slurm_job
#' @param config_path path to the engine config file (e.g. `bbi.yaml` or a
#'   pharos config); defaults to `options('slurmtools.config_path')` so it can be
#'   set globally or scoped with [withr::with_options()]
#' @param account slurm account (`--account`); omitted when `NULL`
#' @param overwrite whether to delete existing model results first
#' @export
submit_slurm_job.bbi_nonmem_model <- function(
  input,
  partition = get_slurm_partitions(),
  ncpu = 1,
  dry_run = FALSE,
  submission_root = getOption("slurmtools.submission_root"),
  template = NULL,
  template_opts = list(),
  ...,
  config_path = getOption("slurmtools.config_path"),
  account = NULL,
  overwrite = FALSE
) {
  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)
  model_path <- input$absolute_model_path

  if (overwrite && fs::dir_exists(model_path)) {
    log4r::info(.le$logger, paste0("Deleting existing directory: ", model_path))
    fs::dir_delete(model_path)
  }

  if (is.null(template)) {
    template <- system.file(
      "templates",
      "nonmem-bbi.tmpl",
      package = "slurmtools"
    )
  }

  job_name <- sprintf("%s-nonmem-run", basename(model_path))
  template_list <- utils::modifyList(
    c(
      base_template_list(job_name, partition, ncpu, account, submission_root),
      list(model_path = model_path, config_path = config_path)
    ),
    template_opts
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", basename(model_path)),
    template = template,
    submission_root = submission_root,
    dry_run = dry_run,
    ...
  )
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.hyperion_nonmem_model <- function(
  input,
  partition = get_slurm_partitions(),
  ncpu = 1,
  dry_run = FALSE,
  submission_root = getOption("slurmtools.submission_root"),
  template = NULL,
  template_opts = list(),
  ...,
  overwrite = FALSE
) {
  if (!requireNamespace("hyperion", quietly = TRUE)) {
    rlang::abort(
      "the hyperion package must be installed to submit hyperion models"
    )
  }
  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)

  log4r::debug(
    .le$logger,
    "delegating hyperion model submission to hyperion::submit_model_to_slurm"
  )

  # hyperion carries its own template and discovers its own pharos config, so
  # slurmtools' template/submission_root/template_opts do not apply here.
  hyperion::submit_model_to_slurm(
    input,
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
  input,
  partition = get_slurm_partitions(),
  ncpu = 1,
  dry_run = FALSE,
  submission_root = getOption("slurmtools.submission_root"),
  template = NULL,
  template_opts = list(),
  ...,
  account = NULL
) {
  # a vector of paths submits one job per path
  if (length(input) > 1) {
    return(lapply(input, function(one) {
      submit_slurm_job(
        one,
        partition = partition,
        ncpu = ncpu,
        dry_run = dry_run,
        submission_root = submission_root,
        template = template,
        template_opts = template_opts,
        account = account,
        ...
      )
    }))
  }

  if (!fs::file_exists(input)) {
    rlang::abort(sprintf("no such file: `%s`", input))
  }

  ext <- tolower(fs::path_ext(input))
  if (ext %in% c("mod", "ctl")) {
    rlang::abort(c(
      sprintf("`%s` is a bare NONMEM control stream, not a submittable object", input),
      i = "read it into a model object first, then submit that:",
      i = "bbr::read_model() or hyperion::read_model()"
    ))
  }

  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)

  if (is.null(template)) {
    template <- switch(
      ext,
      r = system.file("templates", "rscript.tmpl", package = "slurmtools"),
      qmd = system.file("templates", "quarto.tmpl", package = "slurmtools"),
      rlang::abort(c(
        sprintf(
          "don't know how to submit a `.%s` file (supported: .R, .qmd)",
          ext
        ),
        i = "for other file types, supply your own `template` laying out the command"
      ))
    )
  }

  job_name <- basename(input)
  template_list <- utils::modifyList(
    c(
      base_template_list(job_name, partition, ncpu, account, submission_root),
      list(script_path = input)
    ),
    template_opts
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", job_name),
    template = template,
    submission_root = submission_root,
    dry_run = dry_run,
    ...
  )
}

#' @rdname submit_slurm_job
#' @export
submit_slurm_job.default <- function(input, ...) {
  rlang::abort(c(
    sprintf(
      "don't know how to submit an object of class <%s> to slurm",
      paste(class(input), collapse = "/")
    ),
    i = "supply a bbr model, a hyperion model, or a path to a .R/.qmd file"
  ))
}

#' Assemble the template variables common to every shipped template
#'
#' The bbi and character methods need the same core set of whisker variables;
#' this keeps that shape defined in one place. Method-specific values
#' (`model_path`, `config_path`, `script_path`, …) are layered on by the caller.
#'
#' @param job_name job name, also used to derive the log path
#' @param partition resolved partition name
#' @param ncpu number of cpus requested (drives `parallel` / `num_mpi_cpus`)
#' @param account slurm account, or `NULL`
#' @param submission_root directory the job log is written under
#' @return a flat named list of the shared template variables
#' @keywords internal
#' @noRd
base_template_list <- function(
  job_name,
  partition,
  ncpu,
  account,
  submission_root
) {
  list(
    job_name = job_name,
    partition = partition,
    ncpu = ncpu,
    parallel = ncpu > 1,
    num_mpi_cpus = ncpu,
    account = account,
    log_path = file.path(submission_root, sprintf("%s.out", job_name))
  )
}
