test_that("default_template() is main's header shape, account optional", {
  expect_equal(
    format(default_template()),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "#SBATCH --nodes=1",
      "#SBATCH --ntasks=1",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}",
      "{{#account}}",
      "#SBATCH --account={{account}}",
      "{{/account}}"
    )
  )
})

test_that("a default template with a command submits with file, partition and ncpu alone", {
  local_partition_table()
  tmpl <- default_template() |> with_command("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, file = "scripts/sim.R", partition = "cpu2mem4gb", ncpu = 2,
                          submission_root = "root", dry_run = TRUE)
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=sim" %in% script) # the stem of file
  expect_true("#SBATCH --cpus-per-task=2" %in% script)
  expect_false(any(grepl("--account", script)))
  expect_true("#SBATCH --output=root/sim-%j.out" %in% script)
  expect_equal(cmd$args[[2]], "root/sim.sh")
})

test_that("an account, a job name and extra flags go in as with any template", {
  local_partition_table()
  tmpl <- default_template() |> with_command("echo", "{{file}}") |> with_sbatch("time")
  cmd <- submit_slurm_job(tmpl, file = "sim.R", job_name = "mine", account = "proj", time = "01:00:00",
                          partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE)
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=mine" %in% script)
  expect_true("#SBATCH --account=proj" %in% script)
  expect_true("#SBATCH --time=01:00:00" %in% script)
})

test_that("the job-name default follows the flag's placeholder in any template", {
  local_partition_table()
  tmpl <- Template() |> with_job_name("name") |> with_command("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, file = "model/nonmem/1001.mod", dry_run = TRUE)
  expect_match(cmd$template_script, "#SBATCH --job-name=1001\n", fixed = TRUE)
})

test_that("without a file the job name is still a required fill", {
  local_partition_table()
  tmpl <- default_template() |> with_command("echo", "hi")
  expect_error(
    submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE),
    "unfilled placeholders: `\\{\\{job_name\\}\\}`"
  )
})

test_that("ncpu and partition left out keep their defaults: one cpu, the first partition", {
  local_partition_table()
  tmpl <- default_template() |> with_command("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, file = "sim.R", dry_run = TRUE)
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --cpus-per-task=1" %in% script)
  expect_true("#SBATCH --partition=cpu2mem4gb" %in% script) # first row of the fixture table
  expect_equal(cmd$partition, "cpu2mem4gb")
})

test_that("a pre-filled partition or ncpu is not overridden by the default", {
  local_partition_table()
  tmpl <- default_template() |> with_command("echo", "{{file}}") |> fill(partition = "cpu4mem32gb", ncpu = 4)
  cmd <- submit_slurm_job(tmpl, file = "sim.R", dry_run = TRUE)
  expect_match(cmd$template_script, "#SBATCH --cpus-per-task=4", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --partition=cpu4mem32gb", fixed = TRUE)
})

test_that("the defaults leave an optional, unfilled partition flag out", {
  local_partition_table()
  tmpl <- Template() |> with_partition(optional = TRUE) |> with_command("echo", "hi")
  cmd <- submit_slurm_job(tmpl, dry_run = TRUE)
  expect_no_match(cmd$template_script, "--partition", fixed = TRUE)
})
