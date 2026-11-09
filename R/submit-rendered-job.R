#' Render a slurm job script and submit it with sbatch
#'
#' This is the worker behind [submit_slurm_job()]. It fills a whisker template
#' with `template_list`, writes the rendered script to `submission_root`, and
#' submits it via `sbatch`. It knows nothing about what runs on the node: the
#' command lives in the template, and every value it needs (including any
#' paths) is already resolved into `template_list` by the caller. Paths flow
#' through untouched, the template owns any path handling.
#'
#' @param template_list a flat named list of variables to render into the
#'   template. Must include `partition` and `ncpu` for the partition check.
#' @param script_name file name (not path) of the job script to write, e.g.
#'   `"1001.sh"` or `"analysis.R.sh"`
#' @param template path to the whisker template for the job script
#' @param submission_root directory to track job submission scripts and output
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... arguments to pass to [processx::run()]
#'
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition; otherwise the result of
#'   [processx::run()]
#' @keywords internal
#' @noRd
submit_rendered_job <- function(
  template_list,
  script_name,
  template,
  submission_root = getOption("slurmtools.submission_root"),
  dry_run = FALSE,
  ...
) {
  if (is.null(template)) {
    rlang::abort("no slurm job template supplied")
  }
  if (!fs::file_exists(template)) {
    rlang::abort(sprintf("slurm job template not found: `%s`", template))
  }

  log4r::info(
    .le$logger,
    paste0(
      "filling slurm job template with: \n\t",
      paste(
        sapply(names(template_list), function(name) {
          paste0(name, ": ", template_list[[name]])
        }),
        collapse = "\n\t"
      )
    )
  )

  text <- brio::read_file(template)
  rendered <- whisker::whisker.render(unescape_tags(text), template_list)
  res <- sbatch_script(
    rendered,
    script_name = script_name,
    submission_root = submission_root,
    partition = template_list$partition,
    dry_run = dry_run,
    ...
  )
  if (dry_run) {
    return(res)
  }
  # the logs are wherever the rendered script's own #SBATCH lines send them
  job_record(
    res,
    job_name = template_list$job_name,
    partition = template_list$partition,
    output = header_log(rendered, "output", res$job_id),
    error = header_log(rendered, "error", res$job_id)
  )
}

# the lines sbatch reads as directives: everything before the first line that
# is neither blank nor a comment. An `#SBATCH` further down is just a comment
header_lines <- function(rendered) {
  lines <- strsplit(rendered, "\n", fixed = TRUE)[[1]]
  body <- which(!grepl("^\\s*(#|$)", lines))
  if (length(body) > 0) lines[seq_len(body[[1]] - 1)] else lines
}

# the path an `#SBATCH --output=` (or `--error=`) directive of a rendered
# script points at, quotes off and %j resolved; NA when there is none
header_log <- function(rendered, flag, job_id) {
  pattern <- sprintf("^#SBATCH\\s+--%s=", flag)
  line <- grep(pattern, header_lines(rendered), value = TRUE)
  if (length(line) == 0) {
    return(NA_character_)
  }
  value <- trimws(sub(pattern, "", line[[1]]))
  value <- sub("^([\"'])(.*)\\1$", "\\2", value)
  sub("%j", job_id, value, fixed = TRUE)
}

# a script with no --job-name directive would be named by Slurm after the
# file it was submitted from, here a temporary name; give it the name its
# script and logs are kept under instead (sbatch's command line only applies
# where the script says nothing)
job_name_args <- function(rendered, script_name) {
  # `--job-name=x`, `--job-name x` or the short form `-J x`
  if (any(grepl("^#SBATCH\\s+(--job-name([= ]|$)|-J(\\s|$))", header_lines(rendered)))) {
    return(character())
  }
  sprintf("--job-name=%s", sub("\\.sh$", "", script_name))
}

# the one shape every route of submit_slurm_job() returns: sbatch's processx
# result, the job id, and the job's whereabouts (NA where a route cannot
# know), so slurm_job_status(), wait_for_slurm_job(), slurm_job_log() and
# cancel_slurm_job() take any of them (deep-review finding 7)
job_record <- function(res, job_name, partition, output, error) {
  c(
    res[c("status", "stdout", "stderr", "timeout", "job_id")],
    list(job_name = job_name, partition = partition, script = res$script, output = output, error = error)
  )
}

# whisker was written for HTML: a plain `{{tag}}` has `& < > "` in its value
# rewritten as `&amp;` and friends, which a bash script cannot use. A
# triple-brace `{{{tag}}}` is left alone, so every plain tag is widened before
# rendering (deep-review finding 3). Section tags and already-triple tags are
# not touched. The file form does no shell quoting: a template file owns that
unescape_tags <- function(text) {
  gsub("(?<!\\{)\\{\\{\\s*([A-Za-z][A-Za-z0-9_]*)\\s*\\}\\}(?!\\})", "{{{\\1}}}", text, perl = TRUE)
}

#' Write a rendered job script under `submission_root` and hand it to sbatch
#'
#' The last step shared by both forms of [submit_slurm_job()].
#'
#' @param rendered the complete job script, as one string
#' @param script_name file name (not path) of the script to write
#' @param submission_root directory the script (and the job's logs) live in
#' @param partition the partition the header requests, echoed in the dry-run
#'   result; may be `NULL`
#' @param dry_run return the command that would have been invoked, without
#'   invoking
#' @param ... arguments to pass to [processx::run()]
#' @return for a dry run, a list with the sbatch command, its args, the
#'   rendered script, and the partition; otherwise the result of
#'   [processx::run()]
#' @keywords internal
#' @noRd
sbatch_script <- function(
  rendered,
  script_name,
  submission_root,
  partition,
  dry_run = FALSE,
  ...
) {
  if (is.null(submission_root)) {
    rlang::abort(
      "no submission root supplied; set options('slurmtools.submission_root') or pass submission_root"
    )
  }
  script_file_path <- file.path(submission_root, script_name)

  # --parsable makes sbatch answer with the id alone ("2049" or "2049;cluster"),
  # so every route gets the id back and can return it. A dry run shows the
  # script under its plain name; a real submit writes it under a temporary
  # name and keeps it as <job_name>-<job_id>.sh (below)
  sbatch_args <- c("--parsable", job_name_args(rendered, script_name))
  if (dry_run) {
    return(list(
      cmd = Sys.which("sbatch"),
      args = c(sbatch_args, script_file_path),
      template_script = rendered,
      partition = partition
    ))
  }

  if (!fs::dir_exists(submission_root)) {
    log4r::info(.le$logger, "Creating submission root now")
    fs::dir_create(submission_root)
  }
  # the script is written under a temporary name of its own (two submits of
  # the same job at once never share a file) and renamed to
  # <job_name>-<job_id>.sh once sbatch has accepted it, beside the job's
  # <job_name>-<job_id>.out: every job keeps the exact script it ran, a second
  # submit of the same name overwrites nothing, and a refused submit leaves
  # nothing behind (deep-review finding 9, decision 9). sbatch reads the file
  # at submit, so the rename cannot affect the job
  tmp <- tempfile(pattern = paste0(script_name, "."), tmpdir = submission_root, fileext = ".tmp")
  accepted <- FALSE
  on.exit(if (!accepted) unlink(tmp), add = TRUE)
  log4r::debug(.le$logger, paste0("Writing script now: ", tmp))
  brio::write_file(rendered, tmp)
  fs::file_chmod(tmp, "0755")

  log4r::info(.le$logger, "Submitting job now")
  res <- run_slurm("sbatch", c(sbatch_args, tmp), ...)
  job_id <- sub(";.*$", "", trimws(res$stdout))
  if (!grepl("^[0-9]+(_[0-9]+)?$", job_id)) {
    rlang::abort(c(
      "sbatch did not return a job id",
      i = sprintf("sbatch said: %s", trimws(res$stdout))
    ))
  }
  accepted <- TRUE # from here the job exists: its script is kept whatever happens
  script <- script_path_for_job(script_file_path, job_id)
  moved <- tryCatch(
    {
      fs::file_move(tmp, script)
      TRUE
    },
    error = function(e) {
      rlang::warn(c(
        sprintf("job %s was submitted, but its script could not be renamed to `%s`; it is kept as `%s`", job_id, script, tmp),
        x = conditionMessage(e)
      ))
      FALSE
    }
  )
  script <- if (moved) script else tmp
  log4r::debug(.le$logger, paste0("Script kept as: ", script))
  c(res, list(job_id = job_id, script = script))
}

# `<root>/sim.sh` -> `<root>/sim-2049.sh`, the name a submitted job's script is kept under
script_path_for_job <- function(script_file_path, job_id) {
  sub("(\\.sh)?$", sprintf("-%s.sh", job_id), script_file_path)
}
