check_hook_lines <- function(lines) {
  if (!is.character(lines) || length(lines) == 0 || anyNA(lines)) {
    rlang::abort(
      "`lines` must be one or more bash lines, like c(\"module load R\", \"cd {{work_dir}}\")"
    )
  }
  invisible(lines)
}

#' Run bash lines before or after the command
#'
#' @description
#' Hooks are plain bash lines placed around the command: `with_pre_run()`
#' lines run before it, `with_post_run()` lines after it, whichever order the
#' verbs were called in. They go into the script exactly as given — no program
#' resolution, no quoting — so anything bash accepts goes: `module load`,
#' `cd`, `export`, a `curl` to a notification service, `ln -sf` to put a
#' result where downstream tooling expects it, or `bash my-hook.sh`. Lines may
#' hold `{{placeholders}}`, filled like any other with [fill()] or at submit
#' time; `$SLURM_JOB_ID` and the other Slurm variables are there at run time
#' as usual. Calling a verb again appends more lines.
#'
#' Post-run lines run however the command ended: this first version has no
#' exit-status logic. Chain with `&&` / `||` yourself where that matters;
#' success-only and failure-only hooks are a later, opt-in layer.
#'
#' Notifications are a recipe, not a builtin. For ntfy.sh:
#' `with_post_run("curl -s -d '{{job_name}} finished' ntfy.sh/{{ntfy_topic}}")`,
#' then `ntfy_topic` is filled like any placeholder.
#'
#' @param template a [Template()]
#' @param lines character vector of bash lines, one element per line
#'
#' @return the template with the lines added
#'
#' @examples
#' Template() |>
#'   with_job_name() |>
#'   with_partition() |>
#'   with_pre_run(c("module load R/4.5", "cd {{work_dir}}")) |>
#'   with_command("/opt/R/4.5.3/bin/Rscript", "{{file}}") |>
#'   with_post_run("echo \"$SLURM_JOB_ID finished\" >> runs.log")
#' @name with_hooks
NULL

#' @rdname with_hooks
#' @export
with_pre_run <- function(template, lines) {
  check_template(template)
  check_hook_lines(lines)
  template@pre_run <- c(template@pre_run, lines)
  template
}

#' @rdname with_hooks
#' @export
with_post_run <- function(template, lines) {
  check_template(template)
  check_hook_lines(lines)
  template@post_run <- c(template@post_run, lines)
  template
}
