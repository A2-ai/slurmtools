#' A ready-made template to start from
#'
#' @description
#' The header most jobs need, in the shape of the NONMEM template slurmtools
#' has always shipped: one node, one task, `ncpu` cpus on it, a partition, and
#' an account when you give one. Add the command with [with_command()], and
#' anything more with the `with_*()` verbs — [with_sbatch()] takes any sbatch
#' flag.
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
#'
#' @return a [Template()]
#' @examples
#' default_template()
#'
#' rscript <- default_template() |>
#'   with_command("Rscript", "{{file}}") |>
#'   with_sbatch("time")
#' \dontrun{
#' submit_slurm_job(rscript, file = "sim.R", time = "01:00:00")                    # job "sim", 1 cpu, first partition
#' submit_slurm_job(rscript, file = "sim.R", time = "01:00:00", partition = "cpu4mem32gb", ncpu = 4)
#' }
#' @export
default_template <- function() {
  Template() |>
    with_job_name() |>
    with_nodes() |>
    with_ntasks() |>
    with_cpus_per_task("ncpu") |>
    with_partition() |>
    with_account(optional = TRUE) |>
    fill(nodes = 1, ntasks = 1)
}
