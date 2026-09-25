# The registry of common sbatch flags: each maps to the check its fill value
# must pass. A flag here gets a named `with_<flag>()` verb below; any flag,
# listed or not, can be added with `with_sbatch_flag()`. The check runs whichever
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
#' One verb per common sbatch option, each exactly [with_sbatch_flag()] with
#' the flag fixed: `with_nodes(1)` is `#SBATCH --nodes=1`, `with_nodes()` is
#' `#SBATCH --nodes={{nodes}}` to fill later, and `with_nodes("{{n}}")` names
#' that blank. The value is checked whenever it arrives, here or through
#' [fill()]: `--cpus-per-task`, `--nodes`, `--ntasks` and `--ntasks-per-node`
#' need a positive whole number; `--partition`, `--account`, `--gres`,
#' `--output` and `--error` a non-empty string; `--job-name` takes any single
#' value. For any other option use [with_sbatch_flag()]. Pass `optional = TRUE`
#' for a line that should appear only when its placeholder is filled — the
#' usual choice for `with_account()`.
#'
#' @inheritParams with_sbatch_flag
#'
#' @return the template with the flag added
#'
#' @examples
#' Template() |>
#'   with_job_name("sim") |>
#'   with_cpus_per_task(4) |>
#'   with_partition()
#' @name with_flag
NULL

#' @rdname with_flag
#' @export
with_job_name <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "job-name", value, optional)
}

#' @rdname with_flag
#' @export
with_partition <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "partition", value, optional)
}

#' @rdname with_flag
#' @export
with_account <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "account", value, optional)
}

#' @rdname with_flag
#' @export
with_cpus_per_task <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "cpus-per-task", value, optional)
}

#' @rdname with_flag
#' @export
with_nodes <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "nodes", value, optional)
}

#' @rdname with_flag
#' @export
with_ntasks <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "ntasks", value, optional)
}

#' @rdname with_flag
#' @export
with_ntasks_per_node <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "ntasks-per-node", value, optional)
}

#' @rdname with_flag
#' @export
with_gres <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "gres", value, optional)
}

#' @rdname with_flag
#' @export
with_output <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "output", value, optional)
}

#' @rdname with_flag
#' @export
with_error <- function(template, value = NULL, optional = FALSE) {
  with_sbatch_flag(template, "error", value, optional)
}
