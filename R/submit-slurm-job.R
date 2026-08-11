#' Submit a job to slurm
#'
#' @description
#' `submit_slurm_job()` submits a file to slurm: it fills a job **template**
#' with the file and the slurm settings, writes the resulting script under
#' `submission_root`, and hands it to `sbatch`. The command that runs on the
#' compute node lives in the template, not in this function — slurmtools knows
#' nothing about the tool you are running.
#'
#' There is no default template. Every workflow gets its own, created once
#' with [create_slurm_template()] (or written by hand) and reused for every
#' submission after that. A template is a plain bash script with whisker
#' `{{variables}}`.
#'
#' Every template can use `{{file}}`, `{{job_name}}`, `{{partition}}`,
#' `{{ncpu}}`, `{{parallel}}` (true when `ncpu > 1`), `{{num_mpi_cpus}}`,
#' `{{account}}`, and `{{log_path}}`. Anything else a template references is
#' supplied through `template_opts`, which also overrides any of the
#' built-ins. Paths flow into the script exactly as you wrote them — the
#' template owns any path handling.
#'
#' @param file path to the file to submit — the value rendered into the
#'   template's `{{file}}`
#' @param template path to the whisker job template for this workflow; see
#'   [create_slurm_template()]
#' @param partition name of the partition to submit to. The default passes the
#'   full set of partitions; the first is selected.
#' @param ncpu number of cpus to request
#' @param account slurm account (`--account`); omitted when `NULL`
#' @param submission_root directory to track job submission scripts and
#'   output; defaults to `options('slurmtools.submission_root')`, falling back
#'   to `"submission-log"` under the calling directory
#' @param template_opts named list of extra variables to render into the
#'   template (these override the built-in values)
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... additional arguments passed on to [processx::run()]
#'
#' @return for a dry run, a list with the sbatch command, its args, the rendered
#'   script, and the partition; otherwise the result of submitting the job
#'
#' @examples
#' \dontrun{
#' # one-time setup: a template for running R scripts
#' create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
#'
#' # then submit any R script through it
#' submit_slurm_job("scripts/big-simulation.R", template = "rscript.tmpl", ncpu = 4)
#'
#' # a NONMEM-via-bbi workflow: the config travels via template_opts
#' create_slurm_template(
#'   "bbi-nonmem.tmpl",
#'   command = "bbi",
#'   args = c("nonmem", "run", "local", "{{file}}", "--config", "{{config_path}}"),
#'   parallel_args = c("--parallel", "--threads={{ncpu}}")
#' )
#' submit_slurm_job(
#'   "model/nonmem/1001.mod",
#'   template = "bbi-nonmem.tmpl",
#'   ncpu = 2,
#'   template_opts = list(config_path = "model/nonmem/bbi.yaml")
#' )
#' }
#' @export
submit_slurm_job <- function(
  file,
  template,
  partition = get_slurm_partitions(),
  ncpu = 1,
  account = NULL,
  submission_root = getOption(
    "slurmtools.submission_root",
    default = "submission-log"
  ),
  template_opts = list(),
  dry_run = FALSE,
  ...
) {
  if (missing(template) || is.null(template)) {
    rlang::abort(c(
      "`template` is required: every workflow submits through its own job template",
      i = "create one with create_slurm_template(), or point at any whisker bash template"
    ))
  }
  if (!is.character(file) || length(file) != 1) {
    rlang::abort("`file` must be a single path")
  }
  if (!fs::file_exists(file)) {
    rlang::abort(sprintf("no such file: `%s`", file))
  }

  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)

  job_name <- basename(file)
  template_list <- utils::modifyList(
    list(
      file = file,
      job_name = job_name,
      partition = partition,
      ncpu = ncpu,
      parallel = ncpu > 1,
      num_mpi_cpus = ncpu,
      account = account,
      log_path = file.path(submission_root, sprintf("%s.out", job_name))
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
