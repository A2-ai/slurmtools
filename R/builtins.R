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
#' `conditional = "parallel", if_true = c("--parallel", "--threads={{ncpu}}")`
#' and the parallel form of the line is used only for `ncpu > 1`. Sections
#' resolve at submit time, not in `print()` — a template with a conditional
#' still shows both forms and their mustache tags when you print it, the same
#' as an unfilled optional `#SBATCH` line does. Filling a builtin yourself overrides the derived
#' value. Templates used through the file form get the same names.
#'
#' The corpus's NONMEM recipe — a flat symlink to the `.lst` a run writes into
#' its own directory — is one post-run line:
#' `with_post_run("ln", c("-sf", "{{file_dir}}/{{file_stem}}/{{file_stem}}.lst", "{{file_dir}}/{{file_stem}}.lst"))`.
#'
#' @name template_builtins
#' @seealso [with_hooks], [with_command()], [fill()]
NULL

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
  if (!is.null(template) && "cpus-per-task" %in% names(template@sbatch)) {
    cpu_placeholder <- unname(template@sbatch[["cpus-per-task"]])
    if (cpu_placeholder %in% names(fills)) {
      derived$parallel <- isTRUE(suppressWarnings(as.numeric(fills[[cpu_placeholder]])) > 1)
    }
  }
  derived[setdiff(names(derived), names(fills))]
}

# the placeholder that feeds --cpus-per-task, or NA if the template has no such flag;
# for the "how do I unblock {{parallel}}" hint in submit_template()
cpus_per_task_placeholder <- function(template) {
  if ("cpus-per-task" %in% names(template@sbatch)) {
    unname(template@sbatch[["cpus-per-task"]])
  } else {
    NA_character_
  }
}
