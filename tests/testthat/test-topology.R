# --- parse_mem_mb() -------------------------------------------------------------

test_that("parse_mem_mb() defaults to megabytes and honours each suffix", {
  expect_equal(parse_mem_mb("4096"), 4096)
  expect_equal(parse_mem_mb("4096M"), 4096)
  expect_equal(parse_mem_mb("4G"), 4096)
  expect_equal(parse_mem_mb("1T"), 1048576)
  expect_equal(parse_mem_mb("512K"), 0.5)
  expect_equal(parse_mem_mb(8), 8)
})

test_that("parse_mem_mb() ignores case and surrounding space", {
  expect_equal(parse_mem_mb(" 4g "), 4096)
})

test_that("parse_mem_mb() returns NA for anything sbatch's grammar does not cover", {
  expect_true(is.na(parse_mem_mb("4GB")))
  expect_true(is.na(parse_mem_mb("lots")))
  expect_true(is.na(parse_mem_mb("")))
})

# --- --mem against the node's memory --------------------------------------------

test_that("a --mem larger than the node's memory is an error naming a partition that fits", {
  local_partition_table()
  tmpl <- Template() |>
    with_partition() |>
    with_sbatch_flag("mem") |>
    fill(partition = "cpu2mem4gb", mem = "8G")
  expect_error(
    check_slurm_topology(tmpl, "cpu2mem4gb"),
    "exceeds cpu2mem4gb's 3891 MB per node"
  )
  expect_error(check_slurm_topology(tmpl, "cpu2mem4gb"), "cpu4mem32gb")
})

test_that("a --mem the node holds passes, and the partition total never enters into it", {
  local_partition_table()
  fits <- Template() |> with_partition() |> with_sbatch_flag("mem") |>
    fill(partition = "cpu2mem4gb", mem = "3891")
  expect_no_error(check_slurm_topology(fits, "cpu2mem4gb"))
})

test_that("--mem=0 asks for the whole node and always fits", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_sbatch_flag("mem") |>
    fill(partition = "cpu2mem4gb", mem = 0)
  expect_no_error(check_slurm_topology(tmpl, "cpu2mem4gb"))
})

test_that("an unparseable --mem is left for sbatch to reject", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_sbatch_flag("mem") |>
    fill(partition = "cpu2mem4gb", mem = "a lot")
  expect_no_error(check_slurm_topology(tmpl, "cpu2mem4gb"))
})

test_that("the error says so when no partition has enough memory per node", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_sbatch_flag("mem") |>
    fill(partition = "cpu2mem4gb", mem = "2T")
  expect_error(check_slurm_topology(tmpl, "cpu2mem4gb"), "No existing partition has")
})

# --- --gres against a partition that advertises one -----------------------------

test_that("--gres on a partition with no gres is an error listing the ones that have it", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_gres() |>
    fill(partition = "cpu2mem4gb", gres = "gpu:1")
  expect_error(check_slurm_topology(tmpl, "cpu2mem4gb"), "advertises no gres")
  expect_error(check_slurm_topology(tmpl, "cpu2mem4gb"), "partitions with gres: gpu1")
})

test_that("--gres on a partition that advertises one passes", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_gres() |>
    fill(partition = "gpu1", gres = "gpu:1")
  expect_no_error(check_slurm_topology(tmpl, "gpu1"))
})

test_that("the gres error says so when the cluster has no gres at all", {
  local_partition_table(a_partition_table()[1:3, ])
  tmpl <- Template() |> with_partition() |> with_gres() |>
    fill(partition = "cpu2mem4gb", gres = "gpu:1")
  expect_error(check_slurm_topology(tmpl, "cpu2mem4gb"), "No partition on this cluster")
})

# --- whole-node warning for a distributed job ------------------------------------

test_that("a multi-node job that does not fill a node warns, naming what it reserves", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_nodes() |> with_cpus_per_task("{{ncpu}}") |>
    fill(partition = "cpu4mem32gb", nodes = 2, ncpu = 2)
  expect_warning(
    check_slurm_topology(tmpl, "cpu4mem32gb"),
    "does not fill cpu4mem32gb's 4 CPUs per node"
  )
  expect_warning(check_slurm_topology(tmpl, "cpu4mem32gb"), "2 nodes reserve 8 CPUs")
})

test_that("a multi-node job that fills the node is silent", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_nodes() |> with_cpus_per_task("{{ncpu}}") |>
    fill(partition = "cpu4mem32gb", nodes = 4, ncpu = 4)
  expect_no_warning(check_slurm_topology(tmpl, "cpu4mem32gb"))
})

test_that("a single-node job is not a distributed job, whatever cpus-per-task is", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> with_nodes() |> with_cpus_per_task("{{ncpu}}") |>
    fill(partition = "cpu4mem32gb", nodes = 1, ncpu = 1)
  expect_no_warning(check_slurm_topology(tmpl, "cpu4mem32gb"))
})

test_that("a template with none of the three flags is checked and passes", {
  local_partition_table()
  tmpl <- Template() |> with_partition() |> fill(partition = "cpu2mem4gb")
  expect_no_error(check_slurm_topology(tmpl, "cpu2mem4gb"))
})

# --- wired into the submit path --------------------------------------------------

test_that("submit_slurm_job() stops on an over-memory template before reaching sbatch", {
  local_partition_table()
  tmpl <- Template() |>
    with_job_name() |>
    with_partition() |>
    with_sbatch_flag("mem") |>
    with_command("echo", "hi")
  expect_error(
    submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE, slurm_template_opts = list(job_name = "big", mem = "8G")),
    "exceeds cpu2mem4gb's 3891 MB per node"
  )
})

test_that("a template without --partition is not topology-checked", {
  local_partition_table()
  tmpl <- Template() |>
    with_job_name() |>
    with_sbatch_flag("mem") |>
    with_command("echo", "hi")
  dry <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "big", mem = "8G"))
  expect_match(dry$template_script, "#SBATCH --mem=8G", fixed = TRUE)
})
