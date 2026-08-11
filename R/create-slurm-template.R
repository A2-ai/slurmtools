#' Create a slurm job template for your workflow
#'
#' @description
#' Scaffolds the job template that [submit_slurm_job()] fills at submit time:
#' a standard `#SBATCH` header (job name, partition, cpus, optional account,
#' log path) followed by your command. The result is a **plain bash file you
#' own** — open it, edit it, add whatever your workflow needs (environment
#' setup, notifications, cleanup). slurmtools never looks inside it again
#' except to fill the `{{variables}}`.
#'
#' `command` is resolved to an absolute path via `Sys.which()` when the
#' template is created, so the template names the exact binary that will run
#' — and you can see (and change) that choice in the file. A `command` that
#' already contains a `/` is taken as given, skipping the lookup.
#'
#' `args` may reference any variable [submit_slurm_job()] provides —
#' `{{file}}` is the one you almost always want — plus anything you pass at
#' submit time through `template_opts`.
#'
#' When `parallel_args` is supplied, the template gets two variants of the
#' command: the plain one, and one with `parallel_args` appended, used when a
#' job is submitted with `ncpu > 1`.
#'
#' @param path where to write the template file
#' @param command the program the job runs, e.g. `"Rscript"` or `"bbi"`;
#'   resolved via `Sys.which()` unless it already contains a `/`
#' @param args character vector of arguments appended to the command, e.g.
#'   `c("nonmem", "run", "local", "{{file}}")`
#' @param parallel_args extra arguments appended (after `args`) when a job
#'   requests more than one cpu, e.g. `c("--parallel", "--threads={{ncpu}}")`;
#'   `NULL` (the default) writes a single unconditional command line
#' @param overwrite whether to replace an existing file at `path`
#'
#' @return the path to the written template, invisibly
#'
#' @examples
#' \dontrun{
#' # R scripts
#' create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
#'
#' # quarto reports
#' create_slurm_template("quarto.tmpl", command = "quarto", args = c("render", "{{file}}"))
#'
#' # NONMEM via bbi, parallelising when ncpu > 1
#' create_slurm_template(
#'   "bbi-nonmem.tmpl",
#'   command = "bbi",
#'   args = c("nonmem", "run", "local", "{{file}}", "--config", "{{config_path}}"),
#'   parallel_args = c("--parallel", "--threads={{ncpu}}")
#' )
#' }
#' @export
create_slurm_template <- function(
  path,
  command,
  args = character(),
  parallel_args = NULL,
  overwrite = FALSE
) {
  if (!is.character(command) || length(command) != 1) {
    rlang::abort("`command` must be a single program name or path")
  }
  if (fs::file_exists(path) && !overwrite) {
    rlang::abort(c(
      sprintf("template already exists: `%s`", path),
      i = "pass `overwrite = TRUE` to replace it"
    ))
  }

  exe <- if (grepl("/", command, fixed = TRUE)) {
    command
  } else {
    resolve_exe_path(command)
  }

  header <- c(
    "#!/bin/bash",
    "#SBATCH --job-name=\"{{job_name}}\"",
    "#SBATCH --nodes=1",
    "#SBATCH --ntasks=1",
    "#SBATCH --cpus-per-task={{ncpu}}",
    "#SBATCH --partition={{partition}}",
    "{{#account}}",
    "#SBATCH --account={{account}}",
    "{{/account}}",
    "#SBATCH --output={{log_path}}",
    ""
  )

  serial_line <- paste(c(exe, args), collapse = " ")
  body <- if (is.null(parallel_args)) {
    serial_line
  } else {
    c(
      "{{#parallel}}",
      paste(c(exe, args, parallel_args), collapse = " "),
      "{{/parallel}}",
      "{{^parallel}}",
      serial_line,
      "{{/parallel}}"
    )
  }

  brio::write_file(paste(c(header, body, ""), collapse = "\n"), path)
  cli::cli_inform(c(
    "v" = "wrote {.path {path}}",
    "i" = "it is a plain bash script: edit it to fit your workflow",
    "i" = "submit through it with {.code submit_slurm_job(file, template = \"{path}\")}"
  ))
  invisible(path)
}

#' Resolve a tool to the absolute path a template will invoke
#'
#' Templates name the exact binary — resolved on the submitting host when the
#' template is created — rather than trusting the compute node's PATH.
#' Failing here beats writing a template that dies on the node.
#'
#' @param exe command name to resolve, e.g. `"bbi"` or `"Rscript"`
#' @return the absolute path `Sys.which()` found
#' @keywords internal
#' @noRd
resolve_exe_path <- function(exe) {
  path <- unname(Sys.which(exe))
  if (!nzchar(path)) {
    rlang::abort(c(
      sprintf("could not find `%s` on the PATH", exe),
      i = "install it, or pass `command` as an absolute path to the binary"
    ))
  }
  path
}
