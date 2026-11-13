#' Submit a job to slurm
#'
#' @description
#' `submit_slurm_job()` fills a job **template**, writes the resulting script
#' under `submission_root`, and hands it to `sbatch`. The command that runs on
#' the compute node lives in the template, not here — slurmtools knows nothing
#' about the tool you are running. The signature is the one the package has
#' always had; what has widened is what `.mod` may be.
#'
#' **A [Template()]**, the usual case, built from [default_template()].
#' `slurm_template_opts` holds the final fills, `file` first among them:
#' `submit_slurm_job(template, slurm_template_opts = list(file = "sim.R"))`.
#' [slurm_template_opts()] lists what a template can take. `partition` and
#' `ncpu` fill placeholders of those names when the template has them; left
#' out, they keep their defaults (the first available partition, one cpu), so
#' the call above is complete for a default template. `bbi_config_path` fills
#' a `{{bbi_config_path}}` placeholder the same way. Every required
#' placeholder must have a value by then — pre-filled with [fill()] or given
#' here — or the call stops and names the missing ones; a flag added with
#' `optional = TRUE` is simply left out. An unfilled `--job-name` takes the
#' stem of `file` (`sim.R` -> `sim`). Once sbatch has accepted the job, its
#' script is kept as `<submission_root>/<job_name>-<job_id>.sh` (`slurm-job`
#' without a job name), and a template without `--output` or `--error` gets
#' `<submission_root>/<job_name>-%j.out` and `.err`, so every job has its own
#' script and logs, named alike, and a refused submit leaves nothing behind.
#'
#' **A file path**, the file form. The path is rendered into a whisker bash
#' template's `{{file}}` along with `job_name` (the file's stem, `sim.R` -> `sim`), `partition`,
#' `ncpu`, `parallel`, `log_path`, `bbi_config_path`, the [template_builtins],
#' and whatever `slurm_template_opts` adds, which also overrides any of those.
#' The template is `slurm_job_template_path`, written once with
#' [create_slurm_template()] or [write_slurm_template()], or by hand.
#'
#' **A NONMEM model**, the pre-template interface. A `bbi_nonmem_model` from
#' bbr, or a model path with a template written for it (one that takes the
#' model through `{{model_path}}`), runs the original NONMEM/bbi code
#' unchanged, with a once-per-session note pointing at the template way.
#'
#' `overwrite` belongs to that NONMEM form, where it clears the model's output
#' directory. With a template it is refused rather than guessed at: give bbi
#' `--overwrite` in its arguments, or clear the directory yourself.
#'
#' @param .mod what to submit, in one of three forms: a [Template()] (the
#'   argument keeps its old name; pass the template first, as in the
#'   examples); the path of the file to run through `slurm_job_template_path`;
#'   or a NONMEM model (a `bbi_nonmem_model`, or a model path) for the
#'   pre-template interface
#' @param partition name of the partition to submit to, one of those
#'   [get_slurm_partitions()] lists. With a [Template()] it fills the
#'   `{{partition}}` placeholder, taking precedence over a `partition` entry
#'   in `slurm_template_opts`; left out, the first partition is used whenever
#'   the template still needs one.
#' @param ncpu number of cpus to request, a positive whole number. With a
#'   [Template()] it fills the `{{ncpu}}` placeholder, taking precedence over
#'   an `ncpu` entry in `slurm_template_opts`; left out, `1`.
#' @param overwrite pre-template NONMEM form only: whether to delete the
#'   model's existing output directory first
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... arguments passed on to [processx::run()] when sbatch is called
#' @param slurm_job_template_path file form and NONMEM form: path to the
#'   whisker job template; defaults to
#'   `options("slurmtools.slurm_job_template_path")`
#' @param submission_root directory to track job submission scripts and
#'   output; defaults to `options("slurmtools.submission_root")`, falling back
#'   to `"submission-log"` under the calling directory
#' @param bbi_config_path path to a bbi.yaml, filled into
#'   `{{bbi_config_path}}` when the template writes it; defaults to
#'   `options("slurmtools.bbi_config_path")`
#' @param slurm_template_opts named list of `placeholder = value` fills for
#'   the template, applied on top of anything already filled with [fill()].
#'   For a [default_template()] the names are:
#'
#'   | name | fills | if left out |
#'   |---|---|---|
#'   | `file` | `{{file}}` in the command, and the builtins below | required whenever the command writes it |
#'   | `job_name` | `--job-name`, the script name `<job_name>-<job_id>.sh` and the default log names | the stem of `file` (`sim.R` -> `sim`) |
#'   | `partition` | `--partition` | the `partition` argument, else the first partition |
#'   | `ncpu` | `--cpus-per-task` | the `ncpu` argument, else `1` |
#'   | `account` | `--account` | the line is dropped |
#'   | `nodes`, `ntasks` | `--nodes`, `--ntasks` | `1`, filled already |
#'   | `bbi_config_path` | `{{bbi_config_path}}`, when written | the `bbi_config_path` argument |
#'   | `file_dir`, `file_stem`, `file_ext`, `parallel` | see [template_builtins] | derived from `file` and `ncpu` |
#'
#'   plus any other `{{tag}}` the template's flags, command or hooks write.
#'   [slurm_template_opts()] prints this table for the template you have.
#'
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition. Otherwise, whichever form was used,
#'   the result of [processx::run()] on sbatch (`status`, `stdout`, `stderr`,
#'   `timeout`) plus `job_id`, `job_name`, `partition`, `script` (the job's
#'   own copy of its script, `<submission_root>/<job_name>-<job_id>.sh`; the
#'   NONMEM form keeps main's `<model>.sh`), and the `output` and `error`
#'   log paths with `%j` resolved; `NA` where a form cannot know (the file
#'   form knows `output` only when its template wrote `{{log_path}}`, and the
#'   NONMEM form leaves the logs to its template). Hand it to
#'   [slurm_job_status()], [wait_for_slurm_job()], [slurm_job_log()] or
#'   [cancel_slurm_job()].
#'
#' @examples
#' \dontrun{
#' # a template: build once, submit many times
#' rscript <- default_template("Rscript", "{{file}}")
#' job <- submit_slurm_job(rscript, slurm_template_opts = list(file = "sim.R"))
#' job <- submit_slurm_job(rscript, partition = "cpu4mem32gb", ncpu = 4,
#'                         slurm_template_opts = list(file = "sim.R"))
#' wait_for_slurm_job(job)
#' slurm_job_log(job)
#'
#' # NONMEM through bbi, parallel when ncpu > 1
#' bbi <- default_template(
#'   "bbi", c("nonmem", "run", "local", "{{file}}", "--config", "{{bbi_config_path}}"),
#'   conditional("parallel", if_true = c("--parallel", "--threads={{ncpu}}"))
#' )
#' submit_slurm_job(bbi, ncpu = 2, slurm_template_opts = list(
#'   file = "model/nonmem/1001.mod", bbi_config_path = "model/nonmem/bbi.yaml"
#' ))
#'
#' # the file form: a template file written once, any file through it
#' create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
#' submit_slurm_job("scripts/big-simulation.R", slurm_job_template_path = "rscript.tmpl", ncpu = 4)
#' }
#' @export
submit_slurm_job <- function(
  .mod,
  partition = get_slurm_partitions(),
  ncpu = 1,
  overwrite = FALSE,
  dry_run = FALSE,
  ...,
  slurm_job_template_path = getOption("slurmtools.slurm_job_template_path"),
  submission_root = getOption(
    "slurmtools.submission_root",
    default = "submission-log"
  ),
  bbi_config_path = getOption("slurmtools.bbi_config_path"),
  slurm_template_opts = list()
) {
  if (S7::S7_inherits(.mod, Template)) {
    if (!missing(slurm_job_template_path)) {
      rlang::abort(c(
        "`slurm_job_template_path` belongs to the file form: submit_slurm_job(file, slurm_job_template_path = \"path\")",
        i = "a Template is its own template: submit_slurm_job(template, slurm_template_opts = list(file = ...))"
      ))
    }
    check_overwrite(overwrite)
    fills <- check_template_opts(slurm_template_opts)
    # `partition` and `ncpu` fill placeholders of those names; left out, they
    # keep their defaults (the first available partition, one cpu) whenever the
    # template still needs them, as they always did in the file form
    needed <- setdiff(required_placeholders(.mod), c(names(template_fills(.mod)), names(fills)))
    if (!missing(partition)) {
      fills$partition <- partition
    } else if ("partition" %in% needed) {
      fills$partition <- validate_partition(partition)
    }
    if (!missing(ncpu) || "ncpu" %in% needed) {
      fills$ncpu <- ncpu
    }
    if (!is.null(bbi_config_path) && "bbi_config_path" %in% needed) {
      fills$bbi_config_path <- bbi_config_path
    }
    return(submit_template(.mod, fills, submission_root = submission_root, dry_run = dry_run, ...))
  }

  if (is_legacy_submit(.mod, slurm_job_template_path)) {
    warn_legacy_submit()
    # the old code runs sbatch from inside submission_root and hands it
    # <submission_root>/<model>.sh, so a relative root (the default) points
    # at a file that does not exist; make it absolute first (finding 2)
    root <- as.character(fs::path_abs(submission_root))
    res <- submit_slurm_job_legacy(
      .mod,
      partition = partition,
      ncpu = ncpu,
      overwrite = overwrite,
      dry_run = dry_run,
      ...,
      slurm_job_template_path = slurm_job_template_path,
      submission_root = root,
      bbi_config_path = bbi_config_path,
      slurm_template_opts = slurm_template_opts
    )
    if (dry_run) {
      return(res)
    }
    # the old code calls sbatch without --parsable and hands back its raw
    # reply, "Submitted batch job 2049"; read the id off it here so this
    # result works with the follow-up functions like any other (finding 7).
    # R/legacy.R itself stays as it was
    job_id <- regmatches(res$stdout, regexpr("(?<=Submitted batch job )[0-9]+(_[0-9]+)?", res$stdout, perl = TRUE))
    if (length(job_id) != 1) {
      rlang::abort(c(
        "sbatch did not return a job id",
        i = sprintf("sbatch said: %s", trimws(res$stdout))
      ))
    }
    stem <- if (inherits(.mod, "bbi_nonmem_model")) {
      basename(.mod$absolute_model_path)
    } else {
      basename(tools::file_path_sans_ext(.mod))
    }
    return(job_record(
      c(res, list(job_id = job_id, script = file.path(root, sprintf("%s.sh", stem)))),
      job_name = NA_character_, # the template decides it; main's own was <model>-nonmem-run
      partition = partition[[1]],
      output = NA_character_, # the template decides; slurm's default is slurm-<id>.out in the root
      error = NA_character_
    ))
  }

  file <- .mod
  if (!is.character(file) || length(file) != 1) {
    rlang::abort("`.mod` must be a single path, a Template(), or a bbi model")
  }
  if (is.null(slurm_job_template_path)) {
    rlang::abort(c(
      "`slurm_job_template_path` is required: every workflow submits through its own job template",
      i = "create one with create_slurm_template() or write_slurm_template(), or set options(slurmtools.slurm_job_template_path)"
    ))
  }
  if (!fs::file_exists(file)) {
    rlang::abort(sprintf("no such file: `%s`", file))
  }
  check_overwrite(overwrite)
  fills <- check_template_opts(slurm_template_opts)

  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)

  # the file's stem, as the Template route names its jobs: sim.R -> sim
  job_name <- as.character(fs::path_ext_remove(fs::path_file(file)))
  builtin <- list(
    file = file,
    job_name = job_name,
    partition = partition,
    ncpu = ncpu,
    parallel = ncpu > 1,
    num_mpi_cpus = ncpu,
    log_path = file.path(submission_root, sprintf("%s-%%j.out", job_name)) # %j: the job id, so runs never share a log
  )
  if (!is.null(bbi_config_path)) {
    builtin$bbi_config_path <- bbi_config_path
  }
  template_list <- utils::modifyList(
    c(builtin, derived_fills(list(file = file), submission_root)),
    fills
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", job_name),
    template = slurm_job_template_path,
    submission_root = submission_root,
    dry_run = dry_run,
    ...
  )
}

# `overwrite` deletes a model's output directory in the pre-template form; for
# any other file that directory could be anything, so it is refused, not guessed
check_overwrite <- function(overwrite) {
  if (isTRUE(overwrite)) {
    rlang::abort(c(
      "`overwrite` belongs to the pre-template NONMEM form, where it clears the model's output directory",
      i = "with a template, give bbi `--overwrite` in its arguments, or clear the directory yourself"
    ))
  }
  invisible(overwrite)
}

check_template_opts <- function(opts) {
  named <- length(opts) == 0 ||
    (!is.null(names(opts)) && !anyNA(names(opts)) && all(nzchar(names(opts))))
  if (!is.list(opts) || !named) {
    rlang::abort(
      "`slurm_template_opts` must be a named list: slurm_template_opts = list(file = \"sim.R\")"
    )
  }
  opts
}

#' Fill a Template the rest of the way and submit it
#'
#' @param template a [Template()]
#' @param fills named list: the final `placeholder = value` pairs
#' @param submission_root directory to write the job script to
#' @param dry_run return the command instead of invoking sbatch
#' @param ... arguments to pass to [processx::run()]
#' @return see [submit_slurm_job()]
#' @keywords internal
#' @noRd
submit_template <- function(template, fills, submission_root, dry_run, ...) {
  template <- fill(template, !!!fills)

  # an unfilled --job-name takes the file's stem: file = "sim.R" -> sim
  job <- sbatch_entry(template, "job-name")
  filled <- template_fills(template)
  if (!is.null(job) && is.null(filled[[job@fill]]) && !is.null(filled[["file"]])) {
    stem <- c(filled, derived_fills(filled))[["file_stem"]]
    template <- fill(template, !!!rlang::set_names(list(stem), job@fill))
  }
  # a program given as `{{placeholder}}` is looked up now that its fill is known
  template <- resolve_program_fills(template)

  if (length(template@body) == 0) {
    rlang::abort(c(
      "the template has no command to run",
      i = "add one with with_command(template, \"Rscript\", \"{{file}}\")"
    ))
  }
  filled <- template_fills(template)
  derived <- derived_fills(filled, submission_root, template = template)
  # a known switch leaves only the form of its line that applies, so a
  # placeholder written only in the other form is not asked for
  unresolved <- setdiff(required_placeholders(template, c(filled, derived)), c(names(filled), names(derived)))
  if (length(unresolved) > 0) {
    # the first line names what is unfilled as written; the hint names what to
    # give: a missing derived builtin is fixed by filling what it derives from,
    # not itself, and a missing job name follows from `file`. That hint is the
    # list slurm_template_opts() marks required, and a test holds the two
    # together (finding 5)
    cpu_placeholder <- cpus_per_task_placeholder(template)
    job_placeholder <- job_name_placeholder(template)
    takes_stem <- !is.na(job_placeholder) && "file" %in% template_placeholders(template)
    to_fill <- unique(vapply(unresolved, function(name) {
      if (name %in% file_builtins) {
        "file"
      } else if (identical(name, "parallel") && !is.na(cpu_placeholder)) {
        cpu_placeholder
      } else if (identical(name, job_placeholder) && takes_stem) {
        "file"
      } else {
        name
      }
    }, character(1)))
    msg <- c(
      sprintf(
        "the template still has unfilled placeholders: %s",
        paste0("`{{", unresolved, "}}`", collapse = ", ")
      ),
      i = sprintf(
        "fill them at submit: submit_slurm_job(template, slurm_template_opts = list(%s))",
        paste0(to_fill, " = ...", collapse = ", ")
      )
    )
    if (any(unresolved %in% file_builtins)) {
      msg <- c(msg, i = "`{{file_dir}}`, `{{file_stem}}` and `{{file_ext}}` derive from `file`")
    }
    if (takes_stem && job_placeholder %in% unresolved) {
      msg <- c(msg, i = sprintf("`{{%s}}` takes the stem of `file` unless given", job_placeholder))
    }
    if ("parallel" %in% unresolved && !is.na(cpu_placeholder)) {
      msg <- c(msg, i = sprintf("`{{parallel}}` derives from `{{%s}}` (cpus-per-task)", cpu_placeholder))
    }
    rlang::abort(msg)
  }

  partition <- filled_flag(template, "partition")
  if (!is.null(partition)) {
    partition <- validate_partition(partition)
    ncpu <- filled_flag(template, "cpus-per-task")
    if (!is.null(ncpu)) {
      check_slurm_partitions(ncpu, partition)
    }
    check_slurm_topology(template, partition)
  }
  job_name <- filled_flag(template, "job-name")
  if (is.null(job_name)) {
    job_name <- "slurm-job"
  }

  job_name <- fill_text(job_name)
  script_name <- sprintf("%s.sh", job_name)

  # logs default to <submission_root>/<job_name>-%j.{out,err}
  log_stem <- file.path(submission_root, job_name)
  if (is.null(sbatch_entry(template, "output"))) {
    template <- with_output(template, paste0(log_stem, "-%j.out"))
  }
  if (is.null(sbatch_entry(template, "error"))) {
    template <- with_error(template, paste0(log_stem, "-%j.err"))
  }

  # say what the job is made of, as the file form always has: every value,
  # whether given, defaulted or derived (deep-review finding 9)
  filled <- template_fills(template)
  values <- c(filled, derived[setdiff(names(derived), names(filled))])
  log4r::info(
    .le$logger,
    paste0(
      "filling template with: \n\t",
      paste(sprintf("%s: %s", names(values), vapply(values, fill_text, character(1))), collapse = "\n\t")
    )
  )
  log4r::debug(.le$logger, paste0("job name: ", job_name, "; script: ", script_name, "; logs under: ", submission_root))

  # the same renderer as print(): every placeholder is filled by now (checked
  # above), `{{log_dir}}` takes the submission root, and an optional flag left
  # unfilled is dropped. No second pass through whisker, which read `{{ file }}`
  # differently from format() and HTML-escaped values
  rendered <- paste(c(render_template(template, submission_root, final = TRUE), ""), collapse = "\n")
  res <- sbatch_script(
    rendered,
    script_name = script_name,
    submission_root = submission_root,
    partition = partition,
    dry_run = dry_run,
    ...
  )
  if (dry_run) {
    return(res)
  }
  job_record(
    res,
    job_name = job_name,
    partition = partition,
    output = log_path(template, "output", res$job_id, submission_root),
    error = log_path(template, "error", res$job_id, submission_root)
  )
}

# the log a flag points at once %j is known; NA if the flag was left unfilled.
# A value with `{{tags}}` inside it ("{{log_dir}}/{{job_name}}_job%j.out")
# takes the same fills the script did, `log_dir` from the submission root
log_path <- function(template, flag, job_id, submission_root = NULL) {
  value <- filled_flag(template, flag)
  if (is.null(value)) {
    return(NA_character_)
  }
  fills <- template_fills(template)
  fills <- c(fills, derived_fills(fills, submission_root, template = template))
  sub("%j", job_id, substitute_tags(fill_text(value), fills), fixed = TRUE)
}

# the filled value behind a flag, or NULL when the flag is absent or unfilled
filled_flag <- function(template, flag) {
  entry <- sbatch_entry(template, flag)
  if (is.null(entry)) {
    return(NULL)
  }
  template_fills(template)[[entry@fill]]
}
