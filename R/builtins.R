# placeholders slurmtools derives instead of asking for them
file_builtins <- c("file_dir", "file_stem", "file_ext")

#' Placeholders slurmtools fills for you
#'
#' @description
#' whisker has no filters — `{{ file | dirname }}` is not a thing — so the
#' path pieces a hook usually needs are computed in R and filled in as
#' *builtins* whenever a template writes them:
#'
#' | builtin | for `file = "model/nonmem/1001.mod"` |
#' |---|---|
#' | `{{file}}` | `model/nonmem/1001.mod` — the conventional fill, given by you |
#' | `{{file_dir}}` | `model/nonmem` |
#' | `{{file_stem}}` | `1001` |
#' | `{{file_ext}}` | `mod` |
#' | `{{log_dir}}` | the `submission_root` of [submit_slurm_job()], known only at submit |
#' | `{{parallel}}` | `TRUE` once the `--cpus-per-task` fill is filled and `> 1`, else `FALSE` |
#'
#' The first three follow from the `file` fill: fill `file` and they render
#' too, in `print()` and in the submitted script; a template that writes only
#' `{{file_stem}}` still takes `file`. `{{log_dir}}` is filled at submit and
#' stays a `{{log_dir}}` tag in a written template file. `{{parallel}}`
#' follows whichever placeholder [with_cpus_per_task()] (or
#' `with_sbatch_flag("cpus-per-task", ...)`) was given, so it is a whisker
#' *section* rather than a plain tag: give [with_command()] or a hook
#' `conditional("parallel", if_true = c("--parallel", "--threads={{ncpu}}"))`
#' and the parallel form of the line is used only for `ncpu > 1`. While
#' `ncpu` is unknown the template shows both forms and their mustache tags,
#' the same as an unfilled optional `#SBATCH` line does; once it is filled,
#' `print()` shows the form that applies. Filling a builtin yourself
#' overrides the derived value. Templates used through the file form get the
#' same names.
#'
#' The corpus's NONMEM recipe — a flat symlink to the `.lst` a run writes into
#' its own directory — is one post-run line:
#' `with_post_run("ln", c("-sf", "{{file_dir}}/{{file_stem}}/{{file_stem}}.lst", "{{file_dir}}/{{file_stem}}.lst"))`.
#'
#' @name template_builtins
#' @seealso [with_hooks], [with_command()], [fill()]
NULL

# what submit_slurm_job() supplies for each placeholder a template writes but
# nobody fills, as a named vector of descriptions; a placeholder with no entry
# must be given or the submit stops. This is the ONE list of automatic values:
# slurm_template_opts() reads it for its `required` and `default` columns, and
# the test "slurm_template_opts() agrees with submit" holds submit_template()
# to it, so the two cannot drift apart (deep-review finding 5). The dispatcher
# fills `partition` and `ncpu` only under those names and only for those flags,
# whatever flag or line they feed; `parallel` is derived from --cpus-per-task,
# so a template without that flag has to be given it; an optional flag's
# placeholder is "left out" only while nothing in the script writes it too
automatic_defaults <- function(template) {
  option <- template_placeholders(template)
  optional <- optional_flags(template)
  in_script <- script_placeholders(template)
  has_cpu_flag <- !is.null(sbatch_entry(template, "cpus-per-task"))
  out <- character()
  for (name in option) {
    flags <- flags_filled_from(template, name)
    default <- if (name %in% file_builtins) {
      "derived from `file`"
    } else if (identical(name, "log_dir")) {
      "the submission root"
    } else if (identical(name, "parallel") && has_cpu_flag) {
      "TRUE when --cpus-per-task > 1"
    } else if ("job-name" %in% flags && "file" %in% option) {
      "the stem of `file`"
    } else if (identical(name, "ncpu")) {
      "1"
    } else if (identical(name, "partition")) {
      "the first partition"
    } else if (identical(name, "bbi_config_path") && !is.null(getOption("slurmtools.bbi_config_path"))) {
      "the slurmtools.bbi_config_path option"
    } else if (length(flags) > 0 && all(flags %in% optional) && !name %in% in_script) {
      "left out"
    } else {
      NA_character_
    }
    if (!is.na(default)) {
      out[[name]] <- default
    }
  }
  out
}

# the builtin values derivable right now; an explicit fill of the same name wins.
# `template` is optional (the file form has no Template) and only needed for `parallel`.
derived_fills <- function(fills, submission_root = NULL, template = NULL) {
  derived <- list()
  file <- fills[["file"]]
  if (!is.null(file)) {
    file <- fill_text(file)
    derived$file_dir <- as.character(fs::path_dir(file))
    derived$file_stem <- as.character(fs::path_ext_remove(fs::path_file(file)))
    derived$file_ext <- as.character(fs::path_ext(file))
  }
  if (!is.null(submission_root)) {
    derived$log_dir <- submission_root
  }
  cpu <- if (!is.null(template)) sbatch_entry(template, "cpus-per-task")
  if (!is.null(cpu) && cpu@fill %in% names(fills)) {
    derived$parallel <- isTRUE(suppressWarnings(as.numeric(fills[[cpu@fill]])) > 1)
  }
  derived[setdiff(names(derived), names(fills))]
}

# the placeholder that feeds --job-name, or NA if the template has no such flag
job_name_placeholder <- function(template) {
  job <- sbatch_entry(template, "job-name")
  if (is.null(job)) NA_character_ else job@fill
}

# the placeholder that feeds --cpus-per-task, or NA if the template has no such flag;
# for the "how do I unblock {{parallel}}" hint in submit_template()
cpus_per_task_placeholder <- function(template) {
  cpu <- sbatch_entry(template, "cpus-per-task")
  if (is.null(cpu)) NA_character_ else cpu@fill
}
