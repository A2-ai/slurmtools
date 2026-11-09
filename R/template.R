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
  # a header placeholder's value lives on its flag, never in `fills`
  header <- vapply(self@sbatch, function(f) f@fill, character(1))
  doubled <- intersect(names(self@fills), header)
  if (length(doubled) > 0) {
    return(sprintf(
      "fills name header placeholders, whose values belong to their flags: %s",
      paste0("`", doubled, "`", collapse = ", ")
    ))
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
#' Each header line is remembered as what it is made of — the flag, the
#' placeholder it is filled from, its value once given, and whether it is
#' optional — and nothing is substituted until the template is built:
#' `format()`, `print()`, [write_slurm_template()] and [submit_slurm_job()]
#' all render from that record, so a value given to a verb and one given
#' later with [fill()] produce the same line. `NULL`, to a verb or to
#' [fill()], removes the line. The object validates itself whenever it
#' changes: a flag with leading dashes, a duplicated flag, a placeholder that
#' could not be an R argument name, a fill for a placeholder the template
#' does not have, or a value a common flag cannot take (see [with_flag]) is
#' refused on the spot.
#'
#' @param sbatch list of the `#SBATCH` lines in header order, each the flag,
#'   the placeholder it is filled from, its value if given, and whether it is
#'   optional. Built with [with_sbatch_flag()] and the `with_<flag>()` verbs,
#'   never passed directly.
#' @param fills named list of values for the placeholders written in the
#'   script lines, e.g. `list(file = "sim.R")`; a header placeholder's value
#'   lives on its flag. Built with [fill()] rather than passed directly.
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
      S7::class_list,
      default = list(),
      validator = validate_sbatch_flags
    ),
    fills = S7::new_property(
      S7::class_list,
      default = list(),
      validator = validate_fills
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
  # a hand-written whisker section is not a placeholder: nothing renders it
  # now that submit no longer runs whisker over a Template (finding 3)
  if (any(grepl("\\{\\{\\s*[#^/]", x))) {
    rlang::abort(c(
      sprintf("`%s` holds a `{{#section}}` tag, which is not a placeholder", arg),
      i = "a line that changes with a switch is written with conditional(\"name\", if_true = ..., if_false = ...)"
    ))
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
command_entry <- function(command, args, conditionals = list(),
                          launcher = NULL, launcher_args = character(), strict = TRUE) {
  check_program(command, "command", "\"Rscript\"")
  if (S7::S7_inherits(args, Conditional)) {
    rlang::abort(c(
      "`args` is the character vector of arguments; conditionals come after it",
      i = "with_post_run(template, \"rm\", character(), conditional(\"parallel\", if_true = ...))"
    ))
  }
  check_args(args, "args", "c(\"render\", \"{{file}}\")")
  conditionals <- check_conditionals(conditionals)
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
    conditionals = conditionals,
    launcher = if (!is.null(launcher)) program_text(launcher, strict),
    launcher_args = launcher_args
  )
}

# a literal argument with whitespace is one word to the program, so it is
# double-quoted for bash — unless it already is. Double quotes on purpose:
# text written into the template is code, and `$SLURM_JOB_ID` in it expands
shell_arg <- function(x) {
  quoted <- grepl("^([\"']).*\\1$", x)
  needs <- grepl("\\s", x) & !quoted
  x[needs] <- paste0("\"", x[needs], "\"")
  x
}

# the words of a command line with their fills in place, each quoted for bash.
# Quoting happens AFTER substitution, so a filled path with a space arrives as
# one word (deep-review finding 6). An argument that is nothing but a
# `{{placeholder}}` is data: once filled it is single-quoted with shQuote(), so
# no `$`, backtick or `;` in the value is read by bash, and quote characters in
# the value are part of the value. A placeholder inside longer text
# (`--out={{out}}`, `"{{file_stem}} done $SLURM_JOB_ID"`, `{{file_stem}}/worker*`,
# or an author's own `"{{file}}"`) is part of the author's text, which keeps
# shell_arg()'s rule, so the `$VAR` and the glob written there still work. A
# word with a placeholder still unfilled also keeps that rule, so an unfilled
# template prints and writes as before. What counts as a tag is decided from
# the template's word, never from a filled value
fill_words <- function(words, fills) {
  vapply(words, fill_word, character(1), fills = fills, USE.NAMES = FALSE)
}

fill_word <- function(word, fills) {
  names <- tag_names(word)
  if (length(names) == 0) {
    return(shell_arg(word))
  }
  filled <- substitute_tags(word, fills)
  alone <- grepl(paste0("^", tag_re, "$"), word)
  if (!alone || !all(names %in% names(fills))) {
    return(shell_arg(filled))
  }
  quote_fill(filled)
}

# a plain `{{name}}` or `{{ name }}` tag; section tags (`{{#name}}` ...) do not match
tag_re <- "\\{\\{\\s*([A-Za-z][A-Za-z0-9_]*)\\s*\\}\\}"

tag_names <- function(x) {
  gsub("[{}[:space:]]", "", regmatches(x, gregexpr(tag_re, x))[[1]])
}

# every plain tag in `x` replaced by its filled value, in one pass over the
# original text: a value is pasted in as it is and never read as a tag itself
substitute_tags <- function(x, fills) {
  found <- gregexpr(tag_re, x)
  regmatches(x, found) <- lapply(regmatches(x, found), function(tags) {
    names <- gsub("[{}[:space:]]", "", tags)
    vapply(seq_along(tags), function(i) {
      if (names[[i]] %in% names(fills)) fill_text(fills[[names[[i]]]]) else tags[[i]]
    }, character(1))
  })
  x
}

# a value that filled a lone placeholder: left alone when it is plain (letters,
# digits, `_ . / = : , + @ % -`), otherwise single-quoted so bash reads none of
# it, an empty value included; shQuote() handles an embedded `'`
quote_fill <- function(x) {
  if (nzchar(x) && !grepl("[^A-Za-z0-9_./=:,+@%-]", x)) {
    return(x)
  }
  shQuote(x, type = "sh")
}

# the script line(s) one entry renders to, placeholders left as tags. Each
# conditional whose switch is known in `fills` puts the payload of the state
# that applies on the line, in the order given — or takes the line away when
# that payload is NULL. Switches still unknown are left for whisker to pick at
# submit: one open conditional with both payloads writes the two full lines,
# `if_true`'s in `{{#name}}` and `if_false`'s in `{{^name}}`; several write
# one line with each payload in its own inline section; one that can take the
# line away wraps the whole line in the section of the state the line exists in
command_lines <- function(entry, fills = list()) {
  conds <- entry$conditionals
  is_known <- function(cond) cond@name %in% names(fills)
  open <- Filter(Negate(is_known), conds)

  # the words of the line: each known switch's payload in place, `open_as(cond)`
  # standing in for an unknown one; NULL when a known switch takes the line away
  words <- function(open_as) {
    launcher <- if (!is.null(entry$launcher)) fill_word(entry$launcher, fills)
    out <- c(launcher, fill_words(entry$launcher_args, fills), fill_word(entry$exe, fills), fill_words(entry$args, fills))
    for (cond in conds) {
      if (is_known(cond)) {
        extra <- payload(cond, is_truthy(fills[[cond@name]]))
        if (is.null(extra)) {
          return(NULL)
        }
        out <- c(out, fill_words(extra, fills))
      } else {
        out <- c(out, open_as(cond))
      }
    }
    out
  }
  # an inline section (a word inline_section() marked) carries its own leading
  # space, so no joining space goes before it and an unrendered section leaves
  # no gap behind. Only a word that IS a section is touched: the text of every
  # other word, a filled value with `{{#` or a control character included, is
  # left exactly as given
  line <- function(ws) {
    is_section <- startsWith(ws, section_marker)
    ws[is_section] <- substring(ws[is_section], 2)
    sep <- ifelse(is_section, "", " ")
    sep[[1]] <- ""
    paste0(sep, ws, collapse = "")
  }

  if (length(open) == 0) {
    ws <- words(identity)
    return(if (is.null(ws)) character() else line(ws))
  }
  one <- open[[1]]
  if (length(open) == 1 && !is.null(one@if_true) && !is.null(one@if_false)) {
    on <- words(function(cond) fill_words(cond@if_true, fills))
    off <- words(function(cond) fill_words(cond@if_false, fills))
    if (is.null(on)) {
      return(character())
    }
    return(c(
      sprintf("{{#%s}}", one@name), line(on), sprintf("{{/%s}}", one@name),
      sprintf("{{^%s}}", one@name), line(off), sprintf("{{/%s}}", one@name)
    ))
  }
  wrap <- character()
  ws <- words(function(cond) {
    if (is.null(cond@if_false)) {
      wrap <<- c(wrap, sprintf("{{#%s}}", cond@name))
      return(fill_words(cond@if_true, fills))
    }
    if (is.null(cond@if_true)) {
      wrap <<- c(wrap, sprintf("{{^%s}}", cond@name))
      return(fill_words(cond@if_false, fills))
    }
    inline_section(cond, fills)
  })
  if (is.null(ws)) {
    return(character())
  }
  c(wrap, line(ws), rev(sub("^\\{\\{[#^]", "{{/", wrap)))
}

# `{{#name}} a b{{/name}}{{^name}} c{{/name}}`, each part only when it has arguments
inline_section <- function(cond, fills) {
  part <- function(tag, extra) {
    if (length(extra) == 0) {
      return("")
    }
    sprintf("{{%s%s}} %s{{/%s}}", tag, cond@name, paste(fill_words(extra, fills), collapse = " "), cond@name)
  }
  paste0(section_marker, part("#", cond@if_true), part("^", cond@if_false))
}

# prefixed to a generated inline section so line() can tell it from a word; a
# control character no argument contains
section_marker <- "\u0001"

# the lines of a group of entries
group_lines <- function(entries, fills = list()) {
  as.character(unlist(lapply(entries, command_lines, fills = fills), use.names = FALSE))
}

# the lines after the header, in the order they run; with `fills`, a known
# switch leaves only the form that applies
script_lines <- function(x, fills = list()) {
  group_lines(c(x@pre_run, x@body, x@post_run), fills)
}

# `{{name}}`, `{{#name}}`, `{{^name}}`, `{{/name}}` tags written in the script
# lines. Only the switches among `fills` are applied, so a known switch leaves
# the branch that applies while every other tag stays in place: a filled
# value's own text is never read as a tag
script_placeholders <- function(x, fills = list()) {
  switches <- unlist(lapply(c(x@pre_run, x@body, x@post_run), function(entry) {
    vapply(entry$conditionals, function(cond) cond@name, character(1))
  }))
  lines <- script_lines(x, fills[intersect(names(fills), switches)])
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
  header <- vapply(x@sbatch, function(f) f@fill, character(1))
  unique(c(header, written, anchor))
}

# every placeholder that must have a value before the job can be submitted:
# those of required flags, and every one written in the script lines. With
# `fills`, a placeholder written only in the branch a known switch does not
# take is not among them
required_placeholders <- function(x, fills = list()) {
  header <- vapply(x@sbatch, function(f) if (f@optional) NA_character_ else f@fill, character(1))
  written <- script_placeholders(x, fills)
  # a derived builtin nobody filled directly takes `file`, so `file` is required too
  anchor <- if (any(file_builtins %in% setdiff(written, names(fills)))) "file" else character()
  unique(c(header[!is.na(header)], written, anchor))
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
#' value and the line has it, `with_sbatch_flag("time", "01:00:00")` is
#' `#SBATCH --time=01:00:00`; give it nothing and the line is a blank to fill
#' later, `with_sbatch_flag("time")` is `#SBATCH --time={{time}}`; give it a
#' `"{{placeholder}}"` to name that blank yourself, so one value can feed
#' several places; give it `NULL` and the flag is removed, so
#' `with_sbatch_flag(default_template(), "nodes", NULL)` has no `--nodes`
#' line. Adding a flag the template already has replaces its line, its value
#' and its `optional` setting. The common flags also have named verbs,
#' `with_partition()` and friends (see [with_flag]); their values are checked
#' whichever verb added the flag, and whether they arrive here or through
#' [fill()].
#'
#' @param template a [Template()]
#' @param flag an sbatch long option without the leading dashes, e.g.
#'   `"partition"`, `"cpus-per-task"`, `"gres"`
#' @param value the flag's value, a single string or number; a
#'   `"{{placeholder}}"` naming the blank the flag is filled from later; left
#'   out, a blank named after the flag with `-` replaced by `_`, so
#'   `"cpus-per-task"` fills from `{{cpus_per_task}}`; or `NULL` to remove the
#'   flag from the template
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
with_sbatch_flag <- function(template, flag, value, optional = FALSE) {
  check_template(template)
  if (!is.character(flag) || length(flag) != 1 || is.na(flag)) {
    rlang::abort("`flag` must be a single sbatch option name, like \"partition\"")
  }
  if (missing(value)) {
    value <- sprintf("{{%s}}", gsub("-", "_", flag, fixed = TRUE))
  }
  if (is.null(value)) {
    return(without_flag(template, flag))
  }
  if (!is.atomic(value) || length(value) != 1 || is.na(value)) {
    rlang::abort(
      "`value` must be a single value, like 4, or a \"{{placeholder}}\" to fill later"
    )
  }
  if (!is.logical(optional) || length(optional) != 1 || is.na(optional)) {
    rlang::abort("`optional` must be TRUE or FALSE")
  }

  # "{{name}}" names the blank; anything else is the value, kept on the flag
  # until the template is built
  if (is.character(value) && grepl("^\\{\\{.*\\}\\}$", value)) {
    entry <- SbatchFlag(
      flag = flag,
      fill = trimws(sub("^\\{\\{(.*)\\}\\}$", "\\1", value)),
      optional = optional
    )
  } else {
    entry <- SbatchFlag(
      flag = flag,
      fill = gsub("-", "_", flag, fixed = TRUE),
      value = value,
      optional = optional
    )
  }
  sbatch <- template@sbatch
  i <- match(flag, sbatch_flag_names(template))
  if (is.na(i)) {
    sbatch <- c(sbatch, list(entry))
  } else {
    sbatch[[i]] <- entry
  }
  template@sbatch <- sbatch
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
#' `NULL` removes the `#SBATCH` line a placeholder fills:
#' `fill(template, account = NULL)` drops `--account`. The values are kept,
#' not substituted, until the template is built, and a list of them can be
#' spliced in: `fill(template, !!!opts)`. [slurm_template_opts()] lists what
#' there is to fill.
#'
#' @param template a [Template()]
#' @param ... `placeholder = value` pairs. The names are the template's
#'   placeholders: one per `#SBATCH` flag (for [default_template()] those are
#'   `job_name`, `ncpu`, `partition` and `account`, plus `nodes` and `ntasks`
#'   to change the `1` they hold) and every `{{tag}}` written in a command or
#'   hook line, `file` first among them. [slurm_template_opts()] lists them
#'   for the template you have. Each value is a single string or number, or
#'   `NULL` to remove the flag; the common flags check theirs as [with_flag]
#'   describes. `file_dir`, `file_stem`, `file_ext` and `parallel` are
#'   derived for you when the template writes them, and may be given here to
#'   override the derived value (see [template_builtins]).
#'
#' @return the template with those placeholders filled
#'
#' @examples
#' Template() |>
#'   with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
#'   with_sbatch_flag("partition") |>
#'   fill(ncpu = 4, partition = "cpu4mem32gb")
#'
#' default_template() |>
#'   fill(ntasks = NULL) # no --ntasks line
#' @export
fill <- function(template, ...) {
  check_template(template)
  values <- rlang::list2(...)
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

  switches <- switch_names(template)
  for (name in nms) {
    value <- values[[name]]
    flags <- flags_filled_from(template, name)
    if (is.null(value)) {
      if (length(flags) == 0) {
        rlang::abort(c(
          sprintf("`NULL` removes an #SBATCH flag, and `{{%s}}` is written in the script, not a flag", name),
          i = "to leave it blank, do not fill it"
        ))
      }
      for (flag in flags) {
        template <- without_flag(template, flag)
      }
    } else {
      if (name %in% switches) {
        check_switch_value(name, value)
      }
      if (length(flags) > 0) {
        template <- set_flag_value(template, name, value)
      } else {
        fills <- template@fills
        fills[[name]] <- value
        template@fills <- fills
      }
    }
  }
  template
}

#' Add the command a template runs
#'
#' @description
#' Appends the line that runs on the compute node, after the `#SBATCH`
#' header: `command` followed by `args`, one argument per element. Each
#' argument reaches the program as one word. An argument that is just a
#' `"{{placeholder}}"` is single-quoted with [base::shQuote()] once its value is in
#' place, so a path with a space, a `;` or a `$` in the value arrives exactly
#' as given. An argument with text of your own around a placeholder
#' (`"--out={{out}}"`, `"{{file_stem}} done $SLURM_JOB_ID"`) is yours: it is
#' double-quoted only when it has whitespace, so `$SLURM_JOB_ID` and a `*`
#' written there still work on the node. `command` is a program name, a path, or a
#' `{{placeholder}}`. A bare name is resolved to an absolute path with
#' `Sys.which()` when the template is built, so the script names the exact
#' binary that will run and a missing tool fails here rather than on the
#' node; a path is taken as given; a placeholder is filled like any other,
#' with a name or a path, and resolved the same way at submit. `args` may hold
#' `{{placeholders}}` — `{{file}}` is the one you almost always want. Call
#' `with_command()` again to run a second command after the first.
#'
#' Conditionals, made with [conditional()] and given after `args`, switch the
#' line on placeholders: `conditional("parallel", if_true = c("--parallel",
#' "--threads={{ncpu}}"))` appends those two arguments when `{{parallel}}` is
#' true and nothing otherwise. Until a switch is known the template carries
#' both forms, in `{{#parallel}}` and `{{^parallel}}` sections, and the
#' submitted job gets the one that applies; once it is filled or derived, the
#' template shows the one line. Several conditionals put their payloads on
#' the line in the order given, each in its own section while unknown. A
#' payload of `NULL` means the line is not written in that state. The usual
#' switch is the `{{parallel}}` builtin, true when `--cpus-per-task` is filled
#' above one (see [template_builtins]); any other switch is filled with `TRUE`
#' or `FALSE`.
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
#'   word each, e.g. `c("nonmem", "run", "local", "{{file}}")`. Do not add
#'   quotes around a lone placeholder: its value is quoted for the shell when
#'   it is filled
#' @param ... zero or more [conditional()]s that switch the line between two
#'   forms, by position; none for a plain line
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
#' # NONMEM through bbi, parallel only when ncpu > 1, --overwrite when asked
#' default_template() |>
#'   with_command(
#'     "bbi", c("nonmem", "run", "local", "{{file}}", "--config", "{{bbi_config_path}}"),
#'     conditional("parallel", if_true = c("--parallel", "--threads={{ncpu}}")),
#'     conditional("overwrite", if_true = "--overwrite")
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
with_command <- function(template, command, args = character(), ...,
                         launcher = NULL, launcher_args = character()) {
  check_template(template)
  entry <- command_entry(command, args, rlang::list2(...), launcher, launcher_args)
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
#'   `--flag` it fills, or `script` for a command or hook line), `required`
#'   (`TRUE` when [submit_slurm_job()] would stop without it: it has no value
#'   yet and submit does not fill it for you), `value` (the fill so far, `NA`
#'   if none) and `default` (what an unfilled option becomes at submit, `NA`
#'   if it must be given). `required` and `default` come from the same list
#'   submit follows, so the table says exactly what submit enforces
#'
#' @examples
#' default_template("Rscript", "{{file}}") |>
#'   slurm_template_opts()
#' @export
slurm_template_opts <- function(template) {
  check_template(template)
  option <- template_placeholders(template)
  fills <- template_fills(template)
  used_by <- vapply(option, function(name) {
    flags <- flags_filled_from(template, name)
    if (length(flags) > 0) paste0("--", flags, collapse = ", ") else "script"
  }, character(1))
  value <- vapply(option, function(name) {
    if (name %in% names(fills)) fill_text(fills[[name]]) else NA_character_
  }, character(1))
  # what submit supplies for an unfilled option, from the one list submit
  # itself follows; an option with no entry there must be given
  automatic <- automatic_defaults(template)
  default <- vapply(option, function(name) {
    if (name %in% names(automatic)) automatic[[name]] else NA_character_
  }, character(1))
  # the defaults submit will apply are applied here too before asking what is
  # required, so a switch they settle (`parallel` from the default ncpu = 1)
  # settles the same placeholders in both places
  resolved <- fills
  if ("ncpu" %in% names(automatic) && is.null(resolved[["ncpu"]])) {
    resolved$ncpu <- 1
  }
  known <- c(resolved, derived_fills(resolved, template = template))
  tibble::tibble(
    option = option,
    used_by = unname(used_by),
    # "required" means submit would stop if nothing more were given: an option
    # already filled, or one submit fills for you, is not
    required = option %in% required_placeholders(template, known) & is.na(value) & is.na(default),
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
  render_template(x)
}

# the one place a Template becomes script lines: format()/print(),
# write_slurm_template() and submit_template() all come here, so what you
# preview is what is submitted (deep-review finding 3). `final = TRUE` is the
# submit-time render: `submission_root` fills `{{log_dir}}`, and an optional
# header flag still unfilled is dropped instead of left as a `{{#tag}}` section
render_template <- function(x, submission_root = NULL, final = FALSE) {
  fills <- template_fills(x)
  lines <- shebang()
  for (entry in x@sbatch) {
    lines <- c(lines, sbatch_line(entry, fills, final))
  }
  fills <- c(fills, derived_fills(fills, submission_root, template = x))
  for (group in list(x@pre_run, x@body, x@post_run)) {
    rendered <- group_lines(group, fills)
    if (length(rendered) > 0) {
      lines <- c(lines, "", rendered)
    }
  }
  lines
}

# the bash the script asks for: the one on this host's PATH, resolved as every
# program is; `env` finds one at run time when none is found here
shebang <- function() {
  bash <- unname(Sys.which("bash"))
  if (nzchar(bash)) paste0("#!", bash) else "#!/usr/bin/env bash"
}

# one flag's header line(s): filled, a plain line; unfilled, a `{{tag}}` to
# fill, inside a section when the flag is optional (or, at the final render,
# no line at all)
sbatch_line <- function(entry, fills, final = FALSE) {
  value <- fills[[entry@fill]]
  if (!is.null(value)) {
    return(sprintf("#SBATCH --%s=%s", entry@flag, header_value(fill_text(value))))
  }
  if (entry@optional && final) {
    return(character())
  }
  line <- sprintf("#SBATCH --%s={{%s}}", entry@flag, entry@fill)
  if (entry@optional) {
    c(sprintf("{{#%s}}", entry@fill), line, sprintf("{{/%s}}", entry@fill))
  } else {
    line
  }
}

# an `#SBATCH` value with whitespace is double-quoted, as main's template
# wrote `--job-name="{{job_name}}"`: sbatch reads these lines itself and
# otherwise takes the second word as a new directive (deep-review finding 6)
header_value <- function(x) {
  # a value with both whitespace and an inner quote was refused when it was
  # set (validate_sbatch_flag()), so what arrives here can always be written
  if (!grepl("\\s", x) || grepl("^([\"']).*\\1$", x)) {
    return(x)
  }
  sprintf("\"%s\"", x)
}

#' @export
S7::method(print, Template) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
