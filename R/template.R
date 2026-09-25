validate_sbatch_flags <- function(value) {
  flags <- names(value)
  if (length(value) > 0 && (is.null(flags) || anyNA(flags) || !all(nzchar(flags)))) {
    return(
      "must be a named character vector: names are sbatch flags, values are placeholder names"
    )
  }
  bad_flag <- flags[!grepl("^[a-z][a-z0-9-]*$", flags)]
  if (length(bad_flag) > 0) {
    return(sprintf(
      "flags are sbatch long options without the leading dashes, like `cpus-per-task`; not %s",
      paste0("`", bad_flag, "`", collapse = ", ")
    ))
  }
  dup <- unique(flags[duplicated(flags)])
  if (length(dup) > 0) {
    return(sprintf(
      "each flag may appear once; duplicated: %s",
      paste0("`", dup, "`", collapse = ", ")
    ))
  }
  bad_fill <- value[!grepl("^[A-Za-z][A-Za-z0-9_]*$", value)]
  if (length(bad_fill) > 0) {
    return(sprintf(
      "placeholder names must start with a letter and contain only letters, digits and `_`, like `ncpu`; not %s",
      paste0("`", bad_fill, "`", collapse = ", ")
    ))
  }
  NULL
}

validate_fills <- function(value) {
  if (length(value) == 0) {
    return(NULL)
  }
  nms <- names(value)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    return("must be a named list: names are placeholders, values are what fills them")
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0) {
    return(sprintf(
      "each placeholder may be filled once; duplicated: %s",
      paste0("`", dup, "`", collapse = ", ")
    ))
  }
  is_scalar <- vapply(
    value,
    function(v) is.atomic(v) && length(v) == 1 && !is.na(v),
    logical(1)
  )
  if (!all(is_scalar)) {
    return(sprintf(
      "each value must be a single non-missing value (a string or a number); not %s",
      paste0("`", nms[!is_scalar], "`", collapse = ", ")
    ))
  }
  NULL
}

validate_optional <- function(value) {
  if (anyNA(value) || !all(nzchar(value))) {
    return("must name flags, like `account`")
  }
  dup <- unique(value[duplicated(value)])
  if (length(dup) > 0) {
    return(sprintf(
      "each flag may be listed once; duplicated: %s",
      paste0("`", dup, "`", collapse = ", ")
    ))
  }
  NULL
}

# body, pre_run and post_run hold one entry per line to run: the program and
# its arguments, kept apart until the script is rendered (see command_lines())
validate_commands <- function(value) {
  ok <- vapply(
    value,
    function(entry) {
      is.list(entry) && is.character(entry$exe) && length(entry$exe) == 1 && is.character(entry$args)
    },
    logical(1)
  )
  if (!all(ok)) {
    return("must be built with with_command(), with_pre_run() or with_post_run()")
  }
  NULL
}

validate_template <- function(self) {
  orphan <- setdiff(names(self@fills), template_placeholders(self))
  if (length(orphan) > 0) {
    return(sprintf(
      "fills name placeholders the template does not have: %s",
      paste0("`", orphan, "`", collapse = ", ")
    ))
  }
  stray <- setdiff(self@optional, names(self@sbatch))
  if (length(stray) > 0) {
    return(sprintf(
      "optional names flags the template does not have: %s",
      paste0("`", stray, "`", collapse = ", ")
    ))
  }
  for (flag in names(self@sbatch)) {
    placeholder <- self@sbatch[[flag]]
    check <- sbatch_flags[[flag]]
    if (is.null(check) || !placeholder %in% names(self@fills)) {
      next
    }
    value <- self@fills[[placeholder]]
    problem <- check(value)
    if (!is.null(problem)) {
      return(sprintf(
        "`{{%s}}` fills `--%s`, which %s; got %s",
        placeholder,
        flag,
        problem,
        deparse(value)
      ))
    }
  }
  NULL
}

#' A slurm job template, built one piece at a time
#'
#' @description
#' `Template()` starts an empty job template. Each `with_*()` verb adds one
#' `#SBATCH` line whose value is a `{{placeholder}}`; [fill()] gives
#' placeholders their values, a few at a time or all at once;
#' [write_slurm_template()] saves the result as a bash file you can read and
#' edit. `format()` returns the rendered script lines and `print()` shows
#' them, filled values in place and the rest still as `{{placeholders}}`.
#' [default_template()] is the usual starting point: the standard header, and
#' the command if you give it one.
#'
#' A flag added with `optional = TRUE` is emitted only if its placeholder gets
#' a value. Until then it renders inside a `{{#name}} … {{/name}}` section, the
#' form [submit_slurm_job()] already understands, so `--account` can sit in
#' every template and appear only for the jobs that set one.
#'
#' [with_command()] adds the line the job runs, after the header, as a program
#' and its arguments. The arguments may hold `{{placeholders}}` of their own —
#' `{{file}}` is the one you almost always want — and those are filled like
#' any flag's. [submit_slurm_job()] takes the template, fills whatever is
#' left, and refuses to submit while a required placeholder has no value.
#' [with_pre_run()] and [with_post_run()] add lines of your own before and
#' after the command, in the same program-plus-arguments form. A few
#' placeholders are filled for you — `{{file_dir}}`, `{{file_stem}}`,
#' `{{file_ext}}` from `file`, `{{log_dir}}` at submit; see
#' [template_builtins]. [slurm_template_opts()] lists a template's
#' placeholders, what each feeds, and what it falls back to.
#'
#' Flags are held as a named character vector: names are the sbatch long
#' options (`partition`, `cpus-per-task`), values are the placeholder each is
#' filled from. The object validates itself whenever it changes: a flag with
#' leading dashes, a duplicated flag, a placeholder that could not be an R
#' argument name, a fill for a placeholder the template does not have, or a
#' value a common flag cannot take (see [with_flag]) is refused on the spot.
#'
#' @param sbatch named character vector of `#SBATCH` flags in header order,
#'   e.g. `c(partition = "partition", "cpus-per-task" = "ncpu")`. Usually built
#'   with [with_sbatch_flag()] rather than passed directly.
#' @param fills named list of placeholder values already filled in, e.g.
#'   `list(ncpu = 4)`. Usually built with [fill()] rather than passed directly.
#' @param optional character vector of the flags in `sbatch` that are emitted
#'   only when filled, e.g. `"account"`. Usually set through the verbs'
#'   `optional = TRUE` rather than passed directly.
#' @param body list of the commands that follow the header, one entry per
#'   line, each the program and its arguments. Built with [with_command()],
#'   never passed directly.
#' @param pre_run,post_run lists of the commands run before and after the
#'   body, in the same form. Built with [with_pre_run()] and
#'   [with_post_run()], never passed directly.
#'
#' @examples
#' Template() |>
#'   with_sbatch_flag("job-name", "10001") |>
#'   with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
#'   with_sbatch_flag("partition")
#' @export
Template <- S7::new_class(
  "Template",
  package = "slurmtools",
  properties = list(
    sbatch = S7::new_property(
      S7::class_character,
      default = character(),
      validator = validate_sbatch_flags
    ),
    fills = S7::new_property(
      S7::class_list,
      default = list(),
      validator = validate_fills
    ),
    optional = S7::new_property(
      S7::class_character,
      default = character(),
      validator = validate_optional
    ),
    body = S7::new_property(
      S7::class_list,
      default = list(),
      validator = validate_commands
    ),
    pre_run = S7::new_property(
      S7::class_list,
      default = list(),
      validator = validate_commands
    ),
    post_run = S7::new_property(
      S7::class_list,
      default = list(),
      validator = validate_commands
    )
  ),
  validator = validate_template
)

check_template <- function(template, arg = "template") {
  if (!S7::S7_inherits(template, Template)) {
    rlang::abort(sprintf(
      "`%s` must be a Template(), not a <%s>",
      arg,
      class(template)[[1]]
    ))
  }
  invisible(template)
}

# --- commands: program + arguments, rendered to script lines -------------------

# is `x` exactly one `{{placeholder}}` tag, to be filled and resolved at submit?
is_placeholder_tag <- function(x) {
  grepl("^\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}$", x)
}

check_program <- function(x, arg, example) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    rlang::abort(sprintf("`%s` must be a single program name or path, like %s", arg, example))
  }
  if (grepl("\\s", x)) {
    rlang::abort(c(
      sprintf("`%s` is the program alone; its arguments go in `args`, like %s", arg, example),
      i = sprintf("got `%s`", x)
    ))
  }
  invisible(x)
}

check_args <- function(x, arg, example) {
  if (!is.character(x) || anyNA(x)) {
    rlang::abort(sprintf("`%s` must be a character vector, like %s", arg, example))
  }
  invisible(x)
}

# the program as the script will name it: a path is taken as given, a
# `{{placeholder}}` waits for its fill, and a bare name is looked up on the PATH
# now — strictly (an error) for commands, leniently for hooks, whose `module`,
# `cd` or `export` are shell builtins no lookup can find
program_text <- function(program, strict = TRUE) {
  if (grepl("/", program, fixed = TRUE) || is_placeholder_tag(program)) {
    return(program)
  }
  if (strict) {
    return(resolve_exe_path(program))
  }
  found <- Sys.which(program)
  if (nzchar(found)) unname(found) else program
}

# one entry of body / pre_run / post_run, checked
command_entry <- function(command, args, conditional, if_true, if_false,
                          launcher = NULL, launcher_args = character(), strict = TRUE) {
  check_program(command, "command", "\"Rscript\"")
  check_args(args, "args", "c(\"render\", \"{{file}}\")")
  if (!is.null(conditional) &&
    (!is.character(conditional) || length(conditional) != 1 || is.na(conditional) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", conditional))) {
    rlang::abort("`conditional` must be a single placeholder name, like \"parallel\"")
  }
  check_args(if_true, "if_true", "c(\"--parallel\", \"--threads={{ncpu}}\")")
  check_args(if_false, "if_false", "character()")
  if (is.null(conditional) && (length(if_true) > 0 || length(if_false) > 0)) {
    rlang::abort(c(
      "`if_true` / `if_false` need a `conditional`",
      i = "with_command(template, command, args, conditional = \"parallel\", if_true = ...)"
    ))
  }
  if (!is.null(launcher)) {
    check_program(launcher, "launcher", "\"mpirun\"")
  }
  check_args(launcher_args, "launcher_args", "c(\"--bind-to\", \"core\")")
  if (is.null(launcher) && length(launcher_args) > 0) {
    rlang::abort(c(
      "`launcher_args` needs a `launcher`",
      i = "with_command(template, command, args, launcher = \"mpirun\", launcher_args = ...)"
    ))
  }
  list(
    exe = program_text(command, strict),
    args = args,
    conditional = conditional,
    if_true = if_true,
    if_false = if_false,
    launcher = if (!is.null(launcher)) program_text(launcher, strict),
    launcher_args = launcher_args
  )
}

# an argument with whitespace is one word to the program, so it is quoted for
# bash — unless it already is
shell_arg <- function(x) {
  quoted <- grepl("^([\"']).*\\1$", x)
  needs <- grepl("\\s", x) & !quoted
  x[needs] <- paste0("\"", x[needs], "\"")
  x
}

# the script line(s) one entry renders to, placeholders left as tags: one
# line, or — with a conditional — the line with `if_true` inside
# `{{#conditional}}` and the line with `if_false` inside `{{^conditional}}`
command_lines <- function(entry) {
  head <- c(entry$launcher, shell_arg(entry$launcher_args), entry$exe)
  if (is.null(entry$conditional)) {
    return(paste(c(head, shell_arg(entry$args)), collapse = " "))
  }
  c(
    sprintf("{{#%s}}", entry$conditional),
    paste(c(head, shell_arg(c(entry$args, entry$if_true))), collapse = " "),
    sprintf("{{/%s}}", entry$conditional),
    sprintf("{{^%s}}", entry$conditional),
    paste(c(head, shell_arg(c(entry$args, entry$if_false))), collapse = " "),
    sprintf("{{/%s}}", entry$conditional)
  )
}

# the lines of a group of entries
group_lines <- function(entries) {
  as.character(unlist(lapply(entries, command_lines), use.names = FALSE))
}

# the lines after the header, in the order they run
script_lines <- function(x) {
  group_lines(c(x@pre_run, x@body, x@post_run))
}

# `{{name}}`, `{{#name}}`, `{{^name}}`, `{{/name}}` tags written in the script lines
script_placeholders <- function(x) {
  lines <- script_lines(x)
  tags <- regmatches(
    lines,
    gregexpr("\\{\\{[#^/]?\\s*[A-Za-z][A-Za-z0-9_]*\\s*\\}\\}", lines)
  )
  unique(gsub("[{}#^/[:space:]]", "", unlist(tags)))
}

# a template that writes a derived builtin takes `file` as well (see derived_fills())
template_placeholders <- function(x) {
  written <- script_placeholders(x)
  anchor <- if (any(file_builtins %in% written)) "file" else character()
  unique(c(unname(x@sbatch), written, anchor))
}

# every placeholder that must have a value before the job can be submitted:
# those of required flags, and every one written in the script lines
required_placeholders <- function(x) {
  header <- unname(x@sbatch[!names(x@sbatch) %in% x@optional])
  unique(c(header, script_placeholders(x)))
}

# plain `{{name}}` tags replaced by their filled values
substitute_fills <- function(lines, fills) {
  for (name in names(fills)) {
    lines <- gsub(sprintf("{{%s}}", name), fill_text(fills[[name]]), lines, fixed = TRUE)
  }
  lines
}

fill_text <- function(value) {
  format(value, scientific = FALSE, trim = TRUE)
}

# a program given as `{{placeholder}}` is resolved once its fill is known: a
# bare name is looked up on the PATH, a path is taken as given
resolve_program_fills <- function(template) {
  for (entry in c(template@pre_run, template@body, template@post_run)) {
    for (program in c(entry$exe, entry$launcher)) {
      if (!is_placeholder_tag(program)) {
        next
      }
      name <- gsub("[{}]", "", program)
      value <- template@fills[[name]]
      if (!is.null(value) && !grepl("/", value, fixed = TRUE)) {
        template@fills[[name]] <- resolve_exe_path(fill_text(value))
      }
    }
  }
  template
}

#' Add an `#SBATCH` flag to a template
#'
#' @description
#' The blanket verb: any sbatch long option becomes a header line. Give it a
#' value and the line is filled on the spot, `with_sbatch_flag("time",
#' "01:00:00")` is `#SBATCH --time=01:00:00`; give it nothing and the line is a
#' blank to fill later, `with_sbatch_flag("time")` is `#SBATCH
#' --time={{time}}`; give it a `"{{placeholder}}"` to name that blank
#' yourself, so one value can feed several places. Adding a flag the template
#' already has replaces its line, and its `optional` setting. The common
#' flags also have named verbs, `with_partition()` and friends (see
#' [with_flag]); their values are checked whichever verb added the flag, and
#' whether they arrive here or through [fill()].
#'
#' @param template a [Template()]
#' @param flag an sbatch long option without the leading dashes, e.g.
#'   `"partition"`, `"cpus-per-task"`, `"gres"`
#' @param value the flag's value, a single string or number; or a
#'   `"{{placeholder}}"` naming the blank the flag is filled from later;
#'   `NULL` (the default) leaves a blank named after the flag, with `-`
#'   replaced by `_`, so `"cpus-per-task"` fills from `{{cpus_per_task}}`
#' @param optional if `TRUE`, the line is emitted only when its placeholder is
#'   filled; an unfilled optional flag is dropped at submit rather than
#'   reported as missing
#'
#' @return the template with the flag added
#'
#' @examples
#' Template() |>
#'   with_sbatch_flag("partition") |>               # #SBATCH --partition={{partition}}
#'   with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>  # #SBATCH --cpus-per-task={{ncpu}}
#'   with_sbatch_flag("time", "01:00:00")           # #SBATCH --time=01:00:00
#' @export
with_sbatch_flag <- function(template, flag, value = NULL, optional = FALSE) {
  check_template(template)
  if (!is.character(flag) || length(flag) != 1 || is.na(flag)) {
    rlang::abort("`flag` must be a single sbatch option name, like \"partition\"")
  }
  if (!is.null(value) && (!is.atomic(value) || length(value) != 1 || is.na(value))) {
    rlang::abort(
      "`value` must be a single value, like 4, or a \"{{placeholder}}\" to fill later"
    )
  }
  if (!is.logical(optional) || length(optional) != 1 || is.na(optional)) {
    rlang::abort("`optional` must be TRUE or FALSE")
  }

  # "{{name}}" names the blank; anything else fills it
  placeholder <- gsub("-", "_", flag, fixed = TRUE)
  fill_value <- NULL
  if (!is.null(value)) {
    if (is.character(value) && grepl("^\\{\\{.*\\}\\}$", value)) {
      placeholder <- trimws(sub("^\\{\\{(.*)\\}\\}$", "\\1", value))
    } else {
      fill_value <- value
    }
  }

  sbatch <- template@sbatch
  sbatch[[flag]] <- placeholder
  template@sbatch <- sbatch
  opt <- setdiff(template@optional, flag)
  if (optional) {
    opt <- c(opt, flag)
  }
  template@optional <- opt
  if (!is.null(fill_value)) {
    template <- do.call(fill, c(list(template), rlang::set_names(list(fill_value), placeholder)))
  }
  template
}

#' Fill a template's placeholders
#'
#' @description
#' Gives placeholders their values: `fill(template, ncpu = 4)` renders every
#' `{{ncpu}}` as `4`. Fill a few now and the rest later — the template stays a
#' template until every placeholder has a value. Filling a placeholder again
#' replaces the earlier value. A name that is not a placeholder of this
#' template is an error, so a typo cannot silently leave a flag empty.
#' [slurm_template_opts()] lists what there is to fill.
#'
#' @param template a [Template()]
#' @param ... `placeholder = value` pairs; each value a single string or number
#'
#' @return the template with those placeholders filled
#'
#' @examples
#' Template() |>
#'   with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
#'   with_sbatch_flag("partition") |>
#'   fill(ncpu = 4, partition = "cpu4mem32gb")
#' @export
fill <- function(template, ...) {
  check_template(template)
  values <- list(...)
  if (length(values) == 0) {
    return(template)
  }
  nms <- names(values)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    rlang::abort(
      "every argument to `fill()` must be named: fill(template, ncpu = 4)"
    )
  }
  placeholders <- template_placeholders(template)
  unknown <- setdiff(nms, placeholders)
  if (length(unknown) > 0) {
    hint <- if (length(placeholders) == 0) {
      "the template has no placeholders yet: add flags with with_sbatch_flag() or a command with with_command()"
    } else {
      sprintf(
        "placeholders: %s",
        paste0("`", placeholders, "`", collapse = ", ")
      )
    }
    rlang::abort(c(
      sprintf(
        "no placeholder %s in this template",
        paste0("`{{", unknown, "}}`", collapse = ", ")
      ),
      i = hint
    ))
  }

  fills <- template@fills
  fills[nms] <- values
  template@fills <- fills
  template
}

#' Add the command a template runs
#'
#' @description
#' Appends the line that runs on the compute node, after the `#SBATCH`
#' header: `command` followed by `args`, one argument per element. An
#' argument containing whitespace is quoted for bash, so it reaches the
#' program as one word. `command` is a program name, a path, or a
#' `{{placeholder}}`. A bare name is resolved to an absolute path with
#' `Sys.which()` when the template is built, so the script names the exact
#' binary that will run and a missing tool fails here rather than on the
#' node; a path is taken as given; a placeholder is filled like any other,
#' with a name or a path, and resolved the same way at submit. `args` may hold
#' `{{placeholders}}` — `{{file}}` is the one you almost always want. Call
#' `with_command()` again to run a second command after the first.
#'
#' A `conditional` names a placeholder that switches between two forms of
#' the line: with `if_true` appended when the placeholder is true, with
#' `if_false` appended otherwise. The script carries both, in
#' `{{#conditional}}` and `{{^conditional}}` sections, and the submitted job
#' gets the one that applies. The usual switch is the `{{parallel}}` builtin,
#' true when `--cpus-per-task` is filled above one (see [template_builtins]).
#'
#' A distributed tool is usually launcher-plus-binary — `mpirun <flags>
#' distMonolix <flags>` — rather than one program. `launcher` and
#' `launcher_args` prepend that launcher to this command's line, resolved the
#' same way `command` is: which `mpirun` a node finds on its own PATH can be
#' the wrong one (a second MPI stack installed alongside it), so slurmtools
#' shows the exact one it picked rather than trusting the node to find it.
#'
#' @param template a [Template()]
#' @param command the program to run: a name such as `"Rscript"` or `"bbi"`,
#'   a path, or a placeholder such as `"{{bbi}}"`. The program alone:
#'   `"Rscript sim.R"` is an error, its arguments go in `args`
#' @param args character vector of arguments appended to the command, one
#'   word each, e.g. `c("nonmem", "run", "local", "{{file}}")`. An argument
#'   containing a space is quoted for the shell
#' @param conditional a placeholder that decides between `if_true` and
#'   `if_false`, e.g. `"parallel"`; `NULL` (the default) for a plain line
#' @param if_true,if_false character vectors of arguments appended when
#'   `conditional` is true, respectively false, e.g.
#'   `if_true = c("--parallel", "--threads={{ncpu}}")` and
#'   `if_false = character()`
#' @param launcher a launcher program that runs `command` for you, e.g.
#'   `"mpirun"`; `NULL` (the default) runs `command` directly
#' @param launcher_args character vector of arguments appended to the
#'   launcher, before `command`, e.g. `c("--bind-to", "core", "--map-by",
#'   "slot:PE={{ncpu}}")`
#'
#' @return the template with the command line added
#'
#' @examples
#' \dontrun{
#' Template() |>
#'   with_job_name() |>
#'   with_partition() |>
#'   with_command("Rscript", "{{file}}")
#'
#' # NONMEM through bbi, parallel only when ncpu > 1
#' default_template() |>
#'   with_command(
#'     "bbi", c("nonmem", "run", "local", "{{file}}", "--config", "{{bbi_config_path}}"),
#'     conditional = "parallel",
#'     if_true = c("--parallel", "--threads={{ncpu}}")
#'   )
#'
#' # launcher + binary (corpus ex. 2): one line, both resolved and shown
#' Template() |>
#'   with_job_name() |>
#'   with_nodes(2) |>
#'   with_cpus_per_task("{{ncpu}}") |>
#'   with_command(
#'     "/opt/monolix/MonolixSuite2024R1/lib/distMonolix", c("-p", "{{file}}", "--thread", "{{ncpu}}"),
#'     launcher = "mpirun", launcher_args = c("--bind-to", "core", "--map-by", "slot:PE={{ncpu}}")
#'   )
#' }
#' @export
with_command <- function(template, command, args = character(), conditional = NULL,
                         if_true = character(), if_false = character(),
                         launcher = NULL, launcher_args = character()) {
  check_template(template)
  entry <- command_entry(command, args, conditional, if_true, if_false, launcher, launcher_args)
  template@body <- c(template@body, list(entry))
  template
}

#' What goes in `slurm_template_opts`
#'
#' @description
#' One row per placeholder of the template — everything
#' `submit_slurm_job(template, slurm_template_opts = list(...))` or [fill()]
#' can take — with what it feeds, whether the job can be submitted without
#' it, its value so far, and what it falls back to when left out.
#'
#' @param template a [Template()]
#'
#' @return a tibble with columns `option` (the placeholder), `used_by` (the
#'   `--flag` it fills, or `script` for a command or hook line), `required`,
#'   `value` (the fill so far, `NA` if none) and `default` (what an unfilled
#'   option becomes at submit, `NA` if it must be given)
#'
#' @examples
#' default_template("Rscript", "{{file}}") |>
#'   slurm_template_opts()
#' @export
slurm_template_opts <- function(template) {
  check_template(template)
  option <- template_placeholders(template)
  derived <- c(file_builtins, "log_dir", "parallel")
  used_by <- vapply(option, function(name) {
    flags <- names(template@sbatch)[template@sbatch == name]
    if (length(flags) > 0) paste0("--", flags, collapse = ", ") else "script"
  }, character(1))
  value <- vapply(option, function(name) {
    if (name %in% names(template@fills)) fill_text(template@fills[[name]]) else NA_character_
  }, character(1))
  default <- vapply(option, function(name) {
    flags <- names(template@sbatch)[template@sbatch == name]
    if (name %in% file_builtins) {
      "derived from `file`"
    } else if (identical(name, "log_dir")) {
      "the submission root"
    } else if (identical(name, "parallel")) {
      "TRUE when --cpus-per-task > 1"
    } else if ("job-name" %in% flags && "file" %in% option) {
      "the stem of `file`"
    } else if (identical(name, "ncpu") && "cpus-per-task" %in% flags) {
      "1"
    } else if (identical(name, "partition") && "partition" %in% flags) {
      "the first partition"
    } else if (length(flags) > 0 && all(flags %in% template@optional)) {
      "left out"
    } else {
      NA_character_
    }
  }, character(1))
  tibble::tibble(
    option = option,
    used_by = unname(used_by),
    required = option %in% required_placeholders(template) & !option %in% derived,
    value = unname(value),
    default = unname(default)
  )
}

#' Write a template to a file
#'
#' @description
#' Saves the template as it renders now — filled values in place, unfilled
#' placeholders as `{{name}}` — to a plain bash file you own. Open it, edit it,
#' add whatever your workflow needs; slurmtools does not look inside it again
#' except to fill the placeholders. Submit through it with
#' `submit_slurm_job(file, slurm_job_template_path = path)`.
#'
#' @param template a [Template()]
#' @param path where to write the file, e.g. `"rscript.tmpl"`
#' @param overwrite whether to replace an existing file at `path`
#'
#' @return `path`, invisibly
#'
#' @examples
#' \dontrun{
#' default_template("Rscript", "{{file}}") |>
#'   write_slurm_template("rscript.tmpl")
#' }
#' @export
write_slurm_template <- function(template, path, overwrite = FALSE) {
  check_template(template)
  if (fs::file_exists(path) && !overwrite) {
    rlang::abort(c(
      sprintf("template already exists: `%s`", path),
      i = "pass `overwrite = TRUE` to replace it"
    ))
  }
  brio::write_file(paste(c(format(template), ""), collapse = "\n"), path)
  cli::cli_inform(c(
    "v" = "wrote {.path {path}}",
    "i" = "it is a plain bash script: edit it to fit your workflow",
    "i" = "submit through it with {.code submit_slurm_job(file, slurm_job_template_path = \"{path}\")}"
  ))
  invisible(path)
}

#' @export
S7::method(format, Template) <- function(x, ...) {
  lines <- "#!/bin/bash"
  for (flag in names(x@sbatch)) {
    placeholder <- x@sbatch[[flag]]
    if (placeholder %in% names(x@fills)) {
      line <- sprintf("#SBATCH --%s=%s", flag, fill_text(x@fills[[placeholder]]))
    } else {
      line <- sprintf("#SBATCH --%s={{%s}}", flag, placeholder)
      if (flag %in% x@optional) {
        line <- c(sprintf("{{#%s}}", placeholder), line, sprintf("{{/%s}}", placeholder))
      }
    }
    lines <- c(lines, line)
  }
  fills <- c(x@fills, derived_fills(x@fills, template = x))
  for (group in list(x@pre_run, x@body, x@post_run)) {
    if (length(group) > 0) {
      lines <- c(lines, "", substitute_fills(group_lines(group), fills))
    }
  }
  lines
}

#' @export
S7::method(print, Template) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
