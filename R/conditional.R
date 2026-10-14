#' Arguments a command takes one way or the other
#'
#' @description
#' A conditional names a placeholder that switches a command, pre-run or
#' post-run line between two forms: with `if_true` appended when the
#' placeholder is true, with `if_false` appended otherwise. Give one or more
#' to [with_command()], [with_pre_run()] or [with_post_run()] after `args`.
#' Until the switch has a value the template carries both forms, in
#' `{{#name}}` and `{{^name}}` sections, and the submitted job gets the one
#' that applies; once it is known — filled, or derived — the template shows
#' the one line. The usual switch is the `{{parallel}}` builtin, true when
#' `--cpus-per-task` is filled above one (see [template_builtins]); any other
#' switch is a placeholder you fill with `TRUE` or `FALSE`.
#'
#' Either payload may be `NULL`, meaning the line is not written at all in
#' that state: `conditional("parallel", if_true = c("-rf", "{{file_stem}}/worker*"),
#' if_false = NULL)` on a `rm` post-run line removes the worker directories
#' after a parallel run and writes nothing after a serial one. A conditional
#' with nothing in either payload is refused: it would change nothing and
#' still require its switch.
#'
#' @param name the placeholder that decides, e.g. `"parallel"`
#' @param if_true,if_false character vectors of arguments appended when
#'   `name` is true, respectively false. Either may be empty (the plain line)
#'   or `NULL` (no line in that state); at least one must have content
#'
#' @return a conditional, to pass to [with_command()] and the hook verbs
#'
#' @examples
#' \dontrun{
#' # NONMEM through bbi, parallel only when ncpu > 1
#' default_template() |>
#'   with_command(
#'     "bbi", c("nonmem", "run", "local", "{{file}}", "--config", "{{bbi_config_path}}"),
#'     conditional("parallel", if_true = c("--parallel", "--threads={{ncpu}}"))
#'   )
#'
#' # two switches on one line; a post-run line that exists only after a parallel run
#' default_template() |>
#'   with_command(
#'     "bbi", c("nonmem", "run", "local", "{{file}}"),
#'     conditional("parallel", if_true = c("--parallel", "--threads={{ncpu}}")),
#'     conditional("overwrite", if_true = "--overwrite")
#'   ) |>
#'   with_post_run("rm", conditional = conditional("parallel", if_true = c("-rf", "{{file_stem}}/worker*"), if_false = NULL))
#' }
#' @export
conditional <- function(name, if_true = character(), if_false = character()) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    rlang::abort("`name` must be a single placeholder name, like \"parallel\"")
  }
  if (!is.null(if_true)) {
    check_args(if_true, "if_true", "c(\"--parallel\", \"--threads={{ncpu}}\")")
  }
  if (!is.null(if_false)) {
    check_args(if_false, "if_false", "character()")
  }
  if (is.null(if_true) && is.null(if_false)) {
    rlang::abort(c(
      "`if_true` and `if_false` cannot both be NULL: the line would never be written",
      i = "NULL means no line in that state; leave the other payload a character vector"
    ))
  }
  if (length(if_true) == 0 && length(if_false) == 0 && !is.null(if_true) && !is.null(if_false)) {
    rlang::abort(c(
      sprintf("a conditional on `%s` needs arguments in `if_true`, `if_false` or both", name),
      i = sprintf("conditional(\"%s\", if_true = c(\"--parallel\", \"--threads={{ncpu}}\"))", name)
    ))
  }
  Conditional(name = name, if_true = if_true, if_false = if_false)
}

Conditional <- S7::new_class(
  "Conditional",
  package = "slurmtools",
  properties = list(
    name = S7::class_character,
    if_true = S7::new_property(S7::class_character | NULL, default = character()),
    if_false = S7::new_property(S7::class_character | NULL, default = character())
  )
)

# whisker's rule for a section: FALSE, nothing, and an empty value are false
is_truthy <- function(x) {
  !(NROW(x) == 0 || (is.logical(x) && !isTRUE(x[[1]])))
}

# the payload a conditional contributes in one state: a character vector of
# arguments, or NULL when the line is not written in that state
payload <- function(cond, on) {
  if (on) cond@if_true else cond@if_false
}

# the conditionals given to with_command() and the hook verbs, checked: each
# made by conditional(), given by position, each switch once per line
check_conditionals <- function(conds) {
  nms <- names(conds)
  if (!is.null(nms) && any(nzchar(nms))) {
    rlang::abort(c(
      "conditionals are given by position, without names",
      i = "with_command(template, command, args, conditional(\"parallel\", if_true = ...))"
    ))
  }
  made <- vapply(conds, function(x) S7::S7_inherits(x, Conditional), logical(1))
  if (!all(made)) {
    rlang::abort(c(
      "`...` takes conditionals made with conditional()",
      i = "with_command(template, command, args, conditional(\"parallel\", if_true = ...))",
      i = "`launcher` and `launcher_args` are given by name"
    ))
  }
  switches <- vapply(conds, function(x) x@name, character(1))
  twice <- unique(switches[duplicated(switches)])
  if (length(twice) > 0) {
    rlang::abort(c(
      sprintf("each switch may appear once on a line; %s is given twice", paste0("`", twice, "`", collapse = ", ")),
      i = "put both payloads on one conditional(name, if_true = ..., if_false = ...)"
    ))
  }
  unname(conds)
}

# the placeholders that switch a line somewhere in the template
switch_names <- function(template) {
  entries <- c(template@pre_run, template@body, template@post_run)
  unique(unlist(lapply(entries, function(entry) {
    vapply(entry$conditionals, function(cond) cond@name, character(1))
  }), use.names = FALSE))
}

# a switch takes TRUE or FALSE: whisker would read "no", 0 and "" as true
check_switch_value <- function(name, value) {
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(invisible(value))
  }
  rlang::abort(c(
    sprintf("`%s` switches a line between two forms; give it TRUE or FALSE, got %s", name, deparse1(value)),
    i = sprintf("fill(template, %s = FALSE)", name)
  ))
}
