#' A ready-made template to start from
#'
#' @description
#' The header most jobs need, in the shape of the NONMEM template slurmtools
#' has always shipped — one node, one task, `ncpu` cpus on it, a partition,
#' and an account when you give one — plus the command when you pass one:
#' `default_template("Rscript", "{{file}}")` is a complete job. Add anything
#' more with the `with_*()` verbs; [with_sbatch_flag()] takes any sbatch flag,
#' [with_pre_run()] and [with_post_run()] add lines around the command.
#'
#' | placeholder | fills | if you leave it out |
#' |---|---|---|
#' | `job_name` | `--job-name` | the stem of `file` (`sim.R` → `sim`) |
#' | `ncpu` | `--cpus-per-task` | `1` |
#' | `partition` | `--partition` | the first partition `get_slurm_partitions()` lists |
#' | `account` | `--account` | the line is dropped |
#'
#' `--nodes=1` and `--ntasks=1` are filled already; fill `nodes` or `ntasks`
#' again to change them. Logs go to `<submission_root>/<job_name>-%j.out` and
#' `.err` unless you add [with_output()] / [with_error()].
#' [slurm_template_opts()] lists the placeholders of the template you end up
#' with.
#'
#' @param command the program to run, e.g. `"Rscript"`, `"python3"` or
#'   `"bbi"`; `NULL` (the default) for the header alone, to give a command
#'   later with [with_command()]
#' @inheritParams with_command
#'
#' @return a [Template()]
#' @examples
#' default_template()
#'
#' rscript <- default_template("Rscript", "{{file}}") |>
#'   with_sbatch_flag("time")
#' \dontrun{
#' # job "sim", 1 cpu, first partition
#' submit_slurm_job(rscript, slurm_template_opts = list(file = "sim.R", time = "01:00:00"))
#' submit_slurm_job(rscript, partition = "cpu4mem32gb", ncpu = 4,
#'                  slurm_template_opts = list(file = "sim.R", time = "01:00:00"))
#' }
#' @export
default_template <- function(command = NULL, args = character(), conditional = NULL,
                             if_true = character(), if_false = character()) {
  template <- Template() |>
    with_job_name() |>
    with_nodes(1) |>
    with_ntasks(1) |>
    with_cpus_per_task("{{ncpu}}") |>
    with_partition() |>
    with_account(optional = TRUE)
  if (is.null(command)) {
    return(template)
  }
  with_command(template, command, args, conditional = conditional, if_true = if_true, if_false = if_false)
}
