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

test_that("default_template(command, args) is the header plus with_command()", {
  expect_equal(
    format(default_template("echo", c("run", "{{file}}"))),
    format(default_template() |> with_command("echo", c("run", "{{file}}")))
  )
  expect_equal(
    format(default_template("echo", "{{file}}", conditional = "parallel", if_true = "--par")),
    format(default_template() |> with_command("echo", "{{file}}", conditional = "parallel", if_true = "--par"))
  )
  expect_error(default_template("echo", "{{file}}", if_true = "--par"), "need a `conditional`")
  expect_error(default_template("no-such-tool-xyz"), "could not find")
})

test_that("a default template with a command submits with file, partition and ncpu alone", {
  local_partition_table()
  tmpl <- default_template("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 2, submission_root = "root", dry_run = TRUE,
                          slurm_template_opts = list(file = "scripts/sim.R"))
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=sim" %in% script) # the stem of file
  expect_true("#SBATCH --cpus-per-task=2" %in% script)
  expect_false(any(grepl("--account", script)))
  expect_true("#SBATCH --output=root/sim-%j.out" %in% script)
  expect_equal(cmd$args[[2]], "root/sim.sh")
})

test_that("an account, a job name and extra flags go in as with any template", {
  local_partition_table()
  tmpl <- default_template("echo", "{{file}}") |> with_sbatch_flag("time")
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE,
                          slurm_template_opts = list(file = "sim.R", job_name = "mine", account = "proj", time = "01:00:00"))
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=mine" %in% script)
  expect_true("#SBATCH --account=proj" %in% script)
  expect_true("#SBATCH --time=01:00:00" %in% script)
})

test_that("the job-name default follows the flag's placeholder in any template", {
  local_partition_table()
  tmpl <- Template() |> with_job_name("{{name}}") |> with_command("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "model/nonmem/1001.mod"))
  expect_match(cmd$template_script, "#SBATCH --job-name=1001\n", fixed = TRUE)
})

test_that("without a file the job name is still a required fill", {
  local_partition_table()
  tmpl <- default_template("echo", "hi")
  expect_error(
    submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE),
    "unfilled placeholders: `\\{\\{job_name\\}\\}`"
  )
})

test_that("ncpu and partition left out keep their defaults: one cpu, the first partition", {
  local_partition_table()
  tmpl <- default_template("echo", "{{file}}")
  cmd <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R"))
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --cpus-per-task=1" %in% script)
  expect_true("#SBATCH --partition=cpu2mem4gb" %in% script) # first row of the fixture table
  expect_equal(cmd$partition, "cpu2mem4gb")
})

test_that("a pre-filled partition or ncpu is not overridden by the default", {
  local_partition_table()
  tmpl <- default_template("echo", "{{file}}") |> fill(partition = "cpu4mem32gb", ncpu = 4)
  cmd <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R"))
  expect_match(cmd$template_script, "#SBATCH --cpus-per-task=4", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --partition=cpu4mem32gb", fixed = TRUE)
})

test_that("the defaults leave an optional, unfilled partition flag out", {
  local_partition_table()
  tmpl <- Template() |> with_partition(optional = TRUE) |> with_command("echo", "hi")
  cmd <- submit_slurm_job(tmpl, dry_run = TRUE)
  expect_no_match(cmd$template_script, "--partition", fixed = TRUE)
})

test_that("bbi_config_path fills a {{bbi_config_path}} placeholder, from the argument or the option", {
  local_partition_table()
  bbi <- default_template("echo", c("run", "{{file}}", "--config", "{{bbi_config_path}}"))
  cmd <- submit_slurm_job(bbi, bbi_config_path = "cfg/bbi.yaml", dry_run = TRUE, slurm_template_opts = list(file = "1001.mod"))
  expect_match(cmd$template_script, "--config cfg/bbi.yaml", fixed = TRUE)
  withr::local_options(slurmtools.bbi_config_path = "opt/bbi.yaml")
  cmd <- submit_slurm_job(bbi, dry_run = TRUE, slurm_template_opts = list(file = "1001.mod"))
  expect_match(cmd$template_script, "--config opt/bbi.yaml", fixed = TRUE)
  # an explicit fill wins over the option
  cmd <- submit_slurm_job(bbi, dry_run = TRUE, slurm_template_opts = list(file = "1001.mod", bbi_config_path = "mine.yaml"))
  expect_match(cmd$template_script, "--config mine.yaml", fixed = TRUE)
  # a template without the placeholder is not touched by the option
  expect_no_error(submit_slurm_job(default_template("echo", "{{file}}"), dry_run = TRUE, slurm_template_opts = list(file = "a.R")))
})
