#' Create a slurm job template file for your workflow
#'
#' @description
#' Writes [default_template()] with your command as a plain bash file, for the
#' file form of [submit_slurm_job()]:
#' `submit_slurm_job(file, slurm_job_template_path = path)`. The file carries
#' the standard header, `#SBATCH --output={{log_path}}` and the command, with
#' the values the file form supplies left as `{{placeholders}}`: `file`,
#' `job_name`, `partition`, `ncpu`, `parallel`, `account`, `log_path`, the
#' [template_builtins], and anything given through `slurm_template_opts`.
#' With `parallel_args`, the command has a second form used when `ncpu > 1`.
#'
#' This is the same as building the template and calling
#' [write_slurm_template()]; the file is yours to edit afterwards.
#'
#' @param path where to write the template, e.g. `"rscript.tmpl"`
#' @param command the program to run, e.g. `"Rscript"` or `"bbi"`; a bare
#'   name is resolved to an absolute path now, so a missing tool fails here
#' @param args character vector of arguments appended to the command, e.g.
#'   `c("nonmem", "run", "local", "{{file}}")`
#' @param parallel_args arguments appended only when `ncpu > 1`, e.g.
#'   `c("--parallel", "--threads={{ncpu}}")`; `NULL` (the default) for none
#' @param overwrite whether to replace an existing file at `path`
#'
#' @return `path`, invisibly
#'
#' @examples
#' \dontrun{
#' create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
#' create_slurm_template(
#'   "bbi-nonmem.tmpl",
#'   command = "bbi",
#'   args = c("nonmem", "run", "local", "{{file}}", "--config", "{{bbi_config_path}}"),
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
  template <- default_template(
    command,
    args,
    conditional = if (!is.null(parallel_args)) "parallel",
    if_true = if (is.null(parallel_args)) character() else parallel_args
  ) |>
    with_output("{{log_path}}")
  write_slurm_template(template, path, overwrite = overwrite)
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
