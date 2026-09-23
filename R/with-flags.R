# The registry of common sbatch flags: each maps to the check its fill value
# must pass. A flag here gets a named `with_<flag>()` verb below; any flag,
# listed or not, can be added with `with_sbatch()`. The check runs whichever
# verb added the flag, every time the template changes.
needs_positive_whole <- function(value) {
  ok <- is.numeric(value) && value >= 1 && value == trunc(value)
  if (!ok) {
    "needs a positive whole number"
  }
}

needs_nonempty_string <- function(value) {
  if (!is.character(value) || !nzchar(trimws(value))) {
    "needs a non-empty string"
  }
}

any_value <- function(value) {
  NULL
}

sbatch_flags <- list(
  "job-name" = any_value,
  partition = needs_nonempty_string,
  account = needs_nonempty_string,
  "cpus-per-task" = needs_positive_whole,
  nodes = needs_positive_whole,
  ntasks = needs_positive_whole,
  "ntasks-per-node" = needs_positive_whole,
  gres = needs_nonempty_string,
  output = needs_nonempty_string,
  error = needs_nonempty_string
)

#' Add a common `#SBATCH` flag to a template
#'
#' @description
#' One verb per common sbatch option. Each adds
#' `#SBATCH --<flag>={{<fill_name>}}` exactly as [with_sbatch()] would, and
#' the value is checked when it is filled: `--cpus-per-task`, `--nodes`,
#' `--ntasks` and `--ntasks-per-node` need a positive whole number;
#' `--partition`, `--account`, `--gres`, `--output` and `--error` a non-empty
#' string; `--job-name` takes any single value. For any other option use
#' [with_sbatch()]. Pass `optional = TRUE` for a line that should appear only
#' when its placeholder is filled — the usual choice for `with_account()`.
#'
#' @inheritParams with_sbatch
#'
#' @return the template with the flag added
#'
#' @examples
#' Template() |>
#'   with_job_name() |>
#'   with_cpus_per_task("ncpu") |>
#'   with_partition() |>
#'   fill(ncpu = 4)
#' @name with_flag
NULL

#' @rdname with_flag
#' @export
with_job_name <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "job-name", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_partition <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "partition", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_account <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "account", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_cpus_per_task <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "cpus-per-task", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_nodes <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "nodes", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_ntasks <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "ntasks", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_ntasks_per_node <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "ntasks-per-node", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_gres <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "gres", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_output <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "output", fill_name, optional)
}

#' @rdname with_flag
#' @export
with_error <- function(template, fill_name = NULL, optional = FALSE) {
  with_sbatch(template, "error", fill_name, optional)
}
