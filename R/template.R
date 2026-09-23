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
#'
#' A flag added with `optional = TRUE` is emitted only if its placeholder gets
#' a value. Until then it renders inside a `{{#name}} … {{/name}}` section, the
#' form [submit_slurm_job()] already understands, so `--account` can sit in
#' every template and appear only for the jobs that set one.
#'
#' [with_command()] adds the line the job runs, after the header. Its
#' arguments may hold `{{placeholders}}` of their own — `{{file}}` is the one
#' you almost always want — and those are filled like any flag's.
#' [submit_slurm_job()] takes the template, fills whatever is left, and
#' refuses to submit while a required placeholder has no value.
#' [with_pre_run()] and [with_post_run()] add bash lines of your own before
#' and after the command. A few placeholders are filled for you —
#' `{{file_dir}}`, `{{file_stem}}`, `{{file_ext}}` from `file`, `{{log_dir}}`
#' at submit; see [template_builtins].
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
#'   with [with_sbatch()] rather than passed directly.
#' @param fills named list of placeholder values already filled in, e.g.
#'   `list(ncpu = 4)`. Usually built with [fill()] rather than passed directly.
#' @param optional character vector of the flags in `sbatch` that are emitted
#'   only when filled, e.g. `"account"`. Usually set through the verbs'
#'   `optional = TRUE` rather than passed directly.
#' @param body character vector of script lines that follow the header, one
#'   per command. Usually built with [with_command()] rather than passed
#'   directly.
#' @param pre_run,post_run character vectors of bash lines run before and
#'   after the command. Usually built with [with_pre_run()] and
#'   [with_post_run()] rather than passed directly.
#'
#' @examples
#' Template() |>
#'   with_sbatch("job-name") |>
#'   with_sbatch("cpus-per-task", "ncpu") |>
#'   with_sbatch("partition") |>
#'   fill(job_name = "10001")
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
      S7::class_character,
      default = character()
    ),
    pre_run = S7::new_property(
      S7::class_character,
      default = character()
    ),
    post_run = S7::new_property(
      S7::class_character,
      default = character()
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

# the lines after the header, in the order they run
script_lines <- function(x) {
  c(x@pre_run, x@body, x@post_run)
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

#' Add an `#SBATCH` flag to a template
#'
#' @description
#' The blanket verb: any sbatch long option becomes the header line
#' `#SBATCH --<flag>={{<fill_name>}}`. Adding a flag the template already has
#' replaces its placeholder in place, and its `optional` setting. The common
#' flags also have named verbs, `with_partition()` and friends (see
#' [with_flag]); their fill values are checked whichever verb added the flag.
#'
#' @param template a [Template()]
#' @param flag an sbatch long option without the leading dashes, e.g.
#'   `"partition"`, `"cpus-per-task"`, `"gres"`
#' @param fill_name the placeholder the flag is filled from; defaults to the
#'   flag with `-` replaced by `_`, so `"cpus-per-task"` fills from
#'   `{{cpus_per_task}}`. Name it to let one value feed several places:
#'   `with_sbatch("cpus-per-task", "ncpu")`.
#' @param optional if `TRUE`, the line is emitted only when its placeholder is
#'   filled; an unfilled optional flag is dropped at submit rather than
#'   reported as missing
#'
#' @return the template with the flag added
#'
#' @examples
#' Template() |>
#'   with_sbatch("partition") |>
#'   with_sbatch("gres", "gpu")
#' @export
with_sbatch <- function(template, flag, fill_name = NULL, optional = FALSE) {
  check_template(template)
  if (!is.character(flag) || length(flag) != 1 || is.na(flag)) {
    rlang::abort("`flag` must be a single sbatch option name, like \"partition\"")
  }
  if (is.null(fill_name)) {
    fill_name <- gsub("-", "_", flag, fixed = TRUE)
  }
  if (!is.character(fill_name) || length(fill_name) != 1 || is.na(fill_name)) {
    rlang::abort("`fill_name` must be a single placeholder name, like \"ncpu\"")
  }
  if (!is.logical(optional) || length(optional) != 1 || is.na(optional)) {
    rlang::abort("`optional` must be TRUE or FALSE")
  }

  sbatch <- template@sbatch
  sbatch[[flag]] <- fill_name
  template@sbatch <- sbatch
  opt <- setdiff(template@optional, flag)
  if (optional) {
    opt <- c(opt, flag)
  }
  template@optional <- opt
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
#'
#' @param template a [Template()]
#' @param ... `placeholder = value` pairs; each value a single string or number
#'
#' @return the template with those placeholders filled
#'
#' @examples
#' Template() |>
#'   with_sbatch("cpus-per-task", "ncpu") |>
#'   with_sbatch("partition") |>
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
      "the template has no placeholders yet: add flags with with_sbatch() or a command with with_command()"
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
#' Appends the line that runs on the compute node: `command` followed by
#' `args`, after the `#SBATCH` header. `command` is resolved to an absolute
#' path with `Sys.which()` when the template is built, so the script names
#' the exact binary that will run; a `command` that already contains a `/`
#' is taken as given. `args` may hold `{{placeholders}}` — `{{file}}` is the
#' one you almost always want — filled with [fill()] or at submit time. Call
#' `with_command()` again to run a second command after the first.
#'
#' A distributed tool is usually launcher-plus-binary — `mpirun <flags>
#' distMonolix <flags>` — rather than one program. `launcher` and
#' `launcher_args` prepend that launcher to this command's line, resolved to
#' an absolute path the same way `command` is: which `mpirun` a node finds on
#' its own PATH can be the wrong one (a second MPI stack installed alongside
#' it), so slurmtools shows the exact one it picked rather than trusting the
#' node to find it. Which flags a launcher needs (`--bind-to`, `--map-by`,
#' `--thread`) is between you and the launcher; slurmtools only places them.
#'
#' @param template a [Template()]
#' @param command the program to run, e.g. `"Rscript"` or `"bbi"`
#' @param args character vector of arguments appended to the command, e.g.
#'   `c("nonmem", "run", "local", "{{file}}")`
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
#' # launcher + binary (corpus ex. 2): one line, both resolved and shown
#' Template() |>
#'   with_job_name() |>
#'   with_nodes(2) |>
#'   with_cpus_per_task("ncpu") |>
#'   with_command(
#'     "/opt/monolix/MonolixSuite2024R1/lib/distMonolix", c("-p", "{{file}}", "--thread", "{{ncpu}}"),
#'     launcher = "mpirun", launcher_args = c("--bind-to", "core", "--map-by", "slot:PE={{ncpu}}")
#'   )
#' }
#' @export
with_command <- function(template, command, args = character(), launcher = NULL, launcher_args = character()) {
  check_template(template)
  if (!is.character(command) || length(command) != 1 || is.na(command) || !nzchar(command)) {
    rlang::abort("`command` must be a single program name or path, like \"Rscript\"")
  }
  if (!is.character(args)) {
    rlang::abort("`args` must be a character vector, like c(\"render\", \"{{file}}\")")
  }
  if (!is.null(launcher) && (!is.character(launcher) || length(launcher) != 1 || is.na(launcher) || !nzchar(launcher))) {
    rlang::abort("`launcher` must be a single program name or path, like \"mpirun\"")
  }
  if (!is.character(launcher_args)) {
    rlang::abort("`launcher_args` must be a character vector, like c(\"--bind-to\", \"core\")")
  }
  if (is.null(launcher) && length(launcher_args) > 0) {
    rlang::abort(c(
      "`launcher_args` needs a `launcher`",
      i = "with_command(template, command, args, launcher = \"mpirun\", launcher_args = ...)"
    ))
  }
  exe <- if (grepl("/", command, fixed = TRUE)) {
    command
  } else {
    resolve_exe_path(command)
  }
  line_parts <- c(exe, args)
  if (!is.null(launcher)) {
    launcher_exe <- if (grepl("/", launcher, fixed = TRUE)) {
      launcher
    } else {
      resolve_exe_path(launcher)
    }
    line_parts <- c(launcher_exe, launcher_args, exe, args)
  }
  template@body <- c(template@body, paste(line_parts, collapse = " "))
  template
}

#' Write a template to a file
#'
#' @description
#' Saves the template as it renders now — filled values in place, unfilled
#' placeholders as `{{name}}` — to a plain bash file you own. Open it, edit it,
#' add whatever your workflow needs; slurmtools does not look inside it again
#' except to fill the placeholders.
#'
#' @param template a [Template()]
#' @param path where to write the file, e.g. `"rscript.tmpl"`
#' @param overwrite whether to replace an existing file at `path`
#'
#' @return `path`, invisibly
#'
#' @examples
#' \dontrun{
#' Template() |>
#'   with_sbatch("job-name") |>
#'   with_sbatch("partition") |>
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
    "i" = "it is a plain bash script: edit it to fit your workflow"
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
      lines <- c(lines, "", substitute_fills(group, fills))
    }
  }
  lines
}

#' @export
S7::method(print, Template) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
