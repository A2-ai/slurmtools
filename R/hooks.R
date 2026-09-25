#' Run commands before or after the job's command
#'
#' @description
#' Hooks are lines placed around the command, in the same
#' program-plus-arguments form as [with_command()]: `with_pre_run()` lines run
#' before it, `with_post_run()` lines after it, whichever order the verbs
#' were called in. A bare program name is looked up on the PATH and written
#' as its full path when found; one that is not found — `module`, `cd`,
#' `export` and the other shell builtins — is written as given, for bash to
#' resolve on the node. Arguments may hold `{{placeholders}}`, filled like
#' any other with [fill()] or at submit time, and `$SLURM_JOB_ID` and the
#' other Slurm variables are there at run time as usual: an argument with
#' whitespace is wrapped in double quotes, so they still expand. Calling a
#' verb again appends another line. `conditional`, `if_true` and `if_false`
#' work as in [with_command()].
#'
#' Post-run lines run however the command ended: this first version has no
#' exit-status logic. Success-only and failure-only hooks are a later, opt-in
#' layer.
#'
#' Notifications are a recipe, not a builtin. For ntfy.sh:
#' `with_post_run("curl", c("-s", "-d", "{{job_name}} finished", "ntfy.sh/{{ntfy_topic}}"))`,
#' then `ntfy_topic` is filled like any placeholder.
#'
#' @param template a [Template()]
#' @param command the program to run: a name such as `"curl"`, a path, a
#'   shell builtin such as `"module"`, or a placeholder. The program alone:
#'   `"module load R"` is an error, its arguments go in `args`
#' @param args character vector of arguments, one word each, e.g.
#'   `c("load", "R/4.5")`
#' @inheritParams with_command
#'
#' @return the template with the line added
#'
#' @examples
#' Template() |>
#'   with_job_name() |>
#'   with_partition() |>
#'   with_pre_run("module", c("load", "R/4.5")) |>
#'   with_pre_run("cd", "{{work_dir}}") |>
#'   with_command("/opt/R/4.5.3/bin/Rscript", "{{file}}") |>
#'   with_post_run("echo", "$SLURM_JOB_ID finished")
#' @name with_hooks
NULL

#' @rdname with_hooks
#' @export
with_pre_run <- function(template, command, args = character(), conditional = NULL,
                         if_true = character(), if_false = character()) {
  check_template(template)
  entry <- command_entry(command, args, conditional, if_true, if_false, strict = FALSE)
  template@pre_run <- c(template@pre_run, list(entry))
  template
}

#' @rdname with_hooks
#' @export
with_post_run <- function(template, command, args = character(), conditional = NULL,
                          if_true = character(), if_false = character()) {
  check_template(template)
  entry <- command_entry(command, args, conditional, if_true, if_false, strict = FALSE)
  template@post_run <- c(template@post_run, list(entry))
  template
}
