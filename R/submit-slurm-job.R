#' Submit a job to slurm
#'
#' @description
#' `submit_slurm_job()` fills a job **template**, writes the resulting script
#' under `submission_root`, and hands it to `sbatch`. The command that runs on
#' the compute node lives in the template, not in this function — slurmtools
#' knows nothing about the tool you are running.
#'
#' Given a [Template()], the remaining arguments are the final fill:
#' `submit_slurm_job(template, file = "sim.R", ncpu = 4)`. Every required
#' placeholder must have a value by then — pre-filled with [fill()] or passed
#' here — or the call stops and names the missing ones; a flag added with
#' `optional = TRUE` is simply left out. `partition`, `ncpu` and `account`
#' fill placeholders of those names when the template has them; left out,
#' `partition` and `ncpu` keep their defaults (the first available partition,
#' one cpu), so `submit_slurm_job(template, file = "sim.R")` is a complete
#' call for a [default_template()]. An unfilled
#' `--job-name` takes the stem of `file` (`sim.R` -> `sim`). The job
#' script is written as `<job_name>.sh` when the template has a filled
#' `--job-name`, `slurm-job.sh` otherwise. A template without `--output` or
#' `--error` gets `<submission_root>/<job_name>-%j.out` and `.err`, so two
#' submits of the same job never share a log. The call returns a [Job]
#' handle: the id, and where the script and logs are.
#'
#' Given a **file path**, the older form applies: the file is rendered into the
#' template's `{{file}}` along with the slurm settings below.
#'
#' [default_template()] is the usual starting point for a [Template()]; add
#' the command and any further flags to it. For the file form, a template
#' is a plain bash script with whisker `{{variables}}`, created once with
#' [create_slurm_template()] (or written by hand) and reused for every
#' submission after that.
#'
#' Calls written for the pre-template interface keep working: a
#' `bbi_nonmem_model`, `.mod =`, any of `slurm_job_template_path`,
#' `bbi_config_path`, `slurm_template_opts` or `overwrite`, or a file path
#' with no `template` while `options(slurmtools.slurm_job_template_path)` is
#' set, runs the old NONMEM/bbi code unchanged, with a once-per-session
#' warning pointing here.
#'
#' Every template can use `{{file}}`, `{{job_name}}`, `{{partition}}`,
#' `{{ncpu}}`, `{{parallel}}` (true when `ncpu > 1`), `{{num_mpi_cpus}}`,
#' `{{account}}`, and `{{log_path}}`. Anything else a template references is
#' supplied through `template_opts`, which also overrides any of the
#' built-ins. Paths flow into the script exactly as you wrote them — the
#' template owns any path handling.
#'
#' @param x a [Template()] to submit; or, in the file form, the path to the
#'   file to submit — the value rendered into the template's `{{file}}`
#' @param template file form only: path to the whisker job template for this
#'   workflow; see [create_slurm_template()]
#' @param partition name of the partition to submit to. The default passes the
#'   full set of partitions; the first is selected.
#' @param ncpu number of cpus to request
#' @param account slurm account (`--account`); omitted when `NULL`
#' @param submission_root directory to track job submission scripts and
#'   output; defaults to `options('slurmtools.submission_root')`, falling back
#'   to `"submission-log"` under the calling directory
#' @param template_opts file form only: named list of extra variables to
#'   render into the template (these override the built-in values)
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... for a [Template()], the final fills as `placeholder = value`;
#'   for the file form, additional arguments passed on to [processx::run()]
#'
#' @return for a dry run, a list with the sbatch command, its args, the rendered
#'   script, and the partition; for a submitted [Template()], a [Job] handle;
#'   for the file form, the result of [processx::run()] on sbatch
#'
#' @examples
#' \dontrun{
#' # a template object: build once, submit many times
#' rscript <- default_template() |>
#'   with_command("Rscript", "{{file}}")
#' submit_slurm_job(rscript, file = "sim.R", partition = "cpu2mem4gb", ncpu = 2)
#'
#' # the file form
#' # one-time setup: a template for running R scripts
#' create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
#'
#' # then submit any R script through it
#' submit_slurm_job("scripts/big-simulation.R", template = "rscript.tmpl", ncpu = 4)
#'
#' # a NONMEM-via-bbi workflow: the config travels via template_opts
#' create_slurm_template(
#'   "bbi-nonmem.tmpl",
#'   command = "bbi",
#'   args = c("nonmem", "run", "local", "{{file}}", "--config", "{{config_path}}"),
#'   parallel_args = c("--parallel", "--threads={{ncpu}}")
#' )
#' submit_slurm_job(
#'   "model/nonmem/1001.mod",
#'   template = "bbi-nonmem.tmpl",
#'   ncpu = 2,
#'   template_opts = list(config_path = "model/nonmem/bbi.yaml")
#' )
#' }
#' @export
submit_slurm_job <- function(
  x,
  template,
  partition = get_slurm_partitions(),
  ncpu = 1,
  account = NULL,
  submission_root = getOption(
    "slurmtools.submission_root",
    default = "submission-log"
  ),
  template_opts = list(),
  dry_run = FALSE,
  ...
) {
  if (is_legacy_submit(if (missing(x)) NULL else x, missing(template), ...names())) {
    warn_legacy_submit()
    # re-match the call against the old signature, so its positional order holds
    call <- sys.call()
    call[[1]] <- submit_slurm_job_legacy
    return(eval(call, parent.frame()))
  }
  if (S7::S7_inherits(x, Template)) {
    if (!missing(template)) {
      rlang::abort(c(
        "`template` belongs to the file form: submit_slurm_job(file, template = \"path\")",
        i = "a Template is submitted on its own: submit_slurm_job(template, placeholder = value, ...)"
      ))
    }
    if (length(template_opts) > 0) {
      rlang::abort(c(
        "`template_opts` belongs to the file form",
        i = "with a Template, pass the values as fills: submit_slurm_job(template, config_path = \"...\")"
      ))
    }
    fills <- list(...)
    # `partition` and `ncpu` fill placeholders of those names; left out, they
    # keep their defaults (the first available partition, one cpu) whenever the
    # template still needs them, as they always did in the file form
    needed <- setdiff(required_placeholders(x), c(names(x@fills), names(fills)))
    if (!missing(partition)) {
      fills$partition <- partition
    } else if ("partition" %in% needed) {
      fills$partition <- validate_partition(partition)
    }
    if (!missing(ncpu) || "ncpu" %in% needed) {
      fills$ncpu <- ncpu
    }
    if (!missing(account)) {
      fills$account <- account
    }
    return(submit_template(x, fills, submission_root = submission_root, dry_run = dry_run))
  }

  file <- x
  if (missing(template) || is.null(template)) {
    rlang::abort(c(
      "`template` is required: every workflow submits through its own job template",
      i = "create one with create_slurm_template(), or point at any whisker bash template"
    ))
  }
  if (!is.character(file) || length(file) != 1) {
    rlang::abort("`file` must be a single path")
  }
  if (!fs::file_exists(file)) {
    rlang::abort(sprintf("no such file: `%s`", file))
  }

  partition <- validate_partition(partition)
  check_slurm_partitions(ncpu, partition)

  job_name <- basename(file)
  template_list <- utils::modifyList(
    c(
      list(
        file = file,
        job_name = job_name,
        partition = partition,
        ncpu = ncpu,
        parallel = ncpu > 1,
        num_mpi_cpus = ncpu,
        account = account,
        log_path = file.path(submission_root, sprintf("%s.out", job_name))
      ),
      derived_fills(list(file = file), submission_root)
    ),
    template_opts
  )

  submit_rendered_job(
    template_list = template_list,
    script_name = sprintf("%s.sh", job_name),
    template = template,
    submission_root = submission_root,
    dry_run = dry_run,
    ...
  )
}

#' Fill a Template the rest of the way and submit it
#'
#' @param template a [Template()]
#' @param fills named list: the final `placeholder = value` pairs
#' @param submission_root directory to write the job script to
#' @param dry_run return the command instead of invoking sbatch
#' @return see [submit_slurm_job()]
#' @keywords internal
#' @noRd
submit_template <- function(template, fills, submission_root, dry_run) {
  template <- do.call(fill, c(list(template), fills))

  # an unfilled --job-name takes the file's stem: file = "sim.R" -> sim
  job_placeholder <- unname(template@sbatch["job-name"])
  if (!is.na(job_placeholder) && !job_placeholder %in% names(template@fills) &&
      !is.null(template@fills[["file"]])) {
    stem <- c(template@fills, derived_fills(template@fills))[["file_stem"]]
    template <- do.call(fill, c(list(template), rlang::set_names(list(stem), job_placeholder)))
  }

  if (length(template@body) == 0) {
    rlang::abort(c(
      "the template has no command to run",
      i = "add one with with_command(template, \"Rscript\", \"{{file}}\")"
    ))
  }
  derivable <- names(derived_fills(template@fills, submission_root, template = template))
  unresolved <- setdiff(required_placeholders(template), c(names(template@fills), derivable))
  if (length(unresolved) > 0) {
    # a missing derived builtin is fixed by filling what it derives from, not itself
    cpu_placeholder <- cpus_per_task_placeholder(template)
    to_fill <- unique(vapply(unresolved, function(name) {
      if (name %in% file_builtins) {
        "file"
      } else if (identical(name, "parallel") && !is.na(cpu_placeholder)) {
        cpu_placeholder
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
        "fill them at submit: submit_slurm_job(template, %s)",
        paste0(to_fill, " = ...", collapse = ", ")
      )
    )
    if (any(unresolved %in% file_builtins)) {
      msg <- c(msg, i = "`{{file_dir}}`, `{{file_stem}}` and `{{file_ext}}` derive from `file`")
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
  if (is.na(template@sbatch["output"])) {
    template <- with_output(template, "output") |> fill(output = paste0(log_stem, "-%j.out"))
  }
  if (is.na(template@sbatch["error"])) {
    template <- with_error(template, "error") |> fill(error = paste0(log_stem, "-%j.err"))
  }

  # plain `{{tags}}` are already substituted by format(); whisker resolves the
  # sections that remain, dropping any optional flag that was left unfilled
  rendered <- whisker::whisker.render(
    paste(c(format(template), ""), collapse = "\n"),
    c(template@fills, derived_fills(template@fills, submission_root, template = template))
  )
  res <- sbatch_script(
    rendered,
    script_name = script_name,
    submission_root = submission_root,
    partition = partition,
    dry_run = dry_run,
    sbatch_args = "--parsable"
  )
  if (dry_run) {
    return(res)
  }

  job_id <- sub(";.*$", "", trimws(res$stdout)) # "2049" or "2049;cluster"
  if (!grepl("^[0-9]+(_[0-9]+)?$", job_id)) {
    rlang::abort(c(
      "sbatch did not return a job id",
      i = sprintf("sbatch said: %s", trimws(res$stdout))
    ))
  }
  Job(
    job_id = job_id,
    job_name = job_name,
    partition = partition,
    submit_time = Sys.time(),
    script = file.path(submission_root, script_name),
    output = log_path(template, "output", job_id),
    error = log_path(template, "error", job_id),
    sbatch = res
  )
}

# the log a flag points at once %j is known; NA if the flag was left unfilled
log_path <- function(template, flag, job_id) {
  value <- filled_flag(template, flag)
  if (is.null(value)) {
    return(NA_character_)
  }
  sub("%j", job_id, fill_text(value), fixed = TRUE)
}

# the filled value behind a flag, or NULL when the flag is absent or unfilled
filled_flag <- function(template, flag) {
  placeholder <- unname(template@sbatch[flag])
  if (is.na(placeholder) || !placeholder %in% names(template@fills)) {
    return(NULL)
  }
  template@fills[[placeholder]]
}
