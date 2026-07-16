local_submission_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(
    list(slurmtools.submission_root = root),
    .local_envir = env
  )
  root
}

test_that("submit_slurm_job dispatches a .R path to an Rscript command", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  cmd <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE)

  expect_match(cmd$template_script, "Rscript", fixed = TRUE)
  expect_match(cmd$template_script, fs::path_abs(script), fixed = TRUE)
  expect_match(
    cmd$template_script,
    "#SBATCH --partition=cpu2mem4gb",
    fixed = TRUE
  )
  expect_equal(cmd$partition, "cpu2mem4gb")
})

test_that("submit_slurm_job dispatches a .qmd path to a quarto render command", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".qmd", lines = "# a report")

  cmd <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE)

  expect_match(
    cmd$template_script,
    sprintf("render %s", fs::path_abs(script)),
    fixed = TRUE
  )
})

test_that("submit_slurm_job writes and chmods the job script when not a dry run", {
  # a stub sbatch on the PATH keeps this test from submitting a real job
  stub_dir <- withr::local_tempdir()
  brio::write_file(
    "#!/bin/bash\necho Submitted batch job 0\n",
    file.path(stub_dir, "sbatch")
  )
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix")

  root <- local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = FALSE)

  expect_match(res$stdout, "Submitted batch job 0")
  script_file <- file.path(root, sprintf("%s.sh", basename(script)))
  expect_true(fs::file_exists(script_file))
  expect_true(fs::file_access(script_file, mode = "execute"))
})

test_that("submit_slurm_job refuses to guess an engine for bare NONMEM paths", {
  local_submission_root()
  model <- withr::local_tempfile(fileext = ".mod", lines = "$PROBLEM test")

  expect_error(
    submit_slurm_job(model, partition = "cpu2mem4gb", dry_run = TRUE),
    "not treated as a NONMEM model"
  )
})

test_that("submit_slurm_job errors on unsupported file extensions", {
  local_submission_root()
  file <- withr::local_tempfile(fileext = ".txt", lines = "hello")

  expect_error(
    submit_slurm_job(file, partition = "cpu2mem4gb", dry_run = TRUE),
    "don't know how to submit a `.txt` file"
  )
})

test_that("a user-supplied template accepts any file type (power-user contract)", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".py", lines = "print('hi')")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c(
      "#!/bin/bash",
      "#SBATCH --job-name=\"{{job_name}}\"",
      "#SBATCH --partition={{partition}}",
      "source activate {{conda_env}}",
      "python {{script_path}}"
    )
  )

  cmd <- submit_slurm_job(
    script,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    slurm_job_template_path = template,
    slurm_template_opts = list(conda_env = "ml")
  )

  expect_match(cmd$template_script, "source activate ml", fixed = TRUE)
  expect_match(
    cmd$template_script,
    sprintf("python %s", fs::path_abs(script)),
    fixed = TRUE
  )
})

test_that("slurm_template_opts$command overrides the built command", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c("#!/bin/bash", "{{command}}")
  )

  cmd <- submit_slurm_job(
    script,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    slurm_job_template_path = template,
    slurm_template_opts = list(command = "R CMD BATCH my-script.R")
  )

  expect_match(cmd$template_script, "R CMD BATCH my-script.R", fixed = TRUE)
})

test_that("bare NONMEM paths stay rejected without a user-supplied template", {
  local_submission_root()
  model <- withr::local_tempfile(fileext = ".ctl", lines = "$PROBLEM test")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c("#!/bin/bash", "my-tool {{script_path}}")
  )

  expect_error(
    submit_slurm_job(model, partition = "cpu2mem4gb", dry_run = TRUE),
    "not treated as a NONMEM model"
  )
  # but a power user laying out their own template may submit one
  cmd <- submit_slurm_job(
    model,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    slurm_job_template_path = template
  )
  expect_match(cmd$template_script, "my-tool", fixed = TRUE)
})

test_that("submit_slurm_job errors on missing files and vector input", {
  local_submission_root()
  expect_error(
    submit_slurm_job("does-not-exist.R", dry_run = TRUE),
    "no such file"
  )
  expect_error(
    submit_slurm_job(c("a.R", "b.R"), dry_run = TRUE),
    "single file path"
  )
})

test_that("submit_slurm_job.default rejects unknown classes", {
  expect_error(
    submit_slurm_job(42),
    "don't know how to submit an object of class <numeric>"
  )
})

test_that("submit_slurm_job renders a bbi model into the bbi template", {
  local_submission_root()
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c(
      "#!/bin/bash",
      "#SBATCH --job-name=\"{{job_name}}\"",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}",
      "{{#parallel}}",
      "{{bbi_exe_path}} nonmem run local {{model_path}}.mod --parallel --threads={{ncpu}} --config {{bbi_config_path}}",
      "{{/parallel}}",
      "{{^parallel}}",
      "{{bbi_exe_path}} nonmem run local {{model_path}}.mod --config {{bbi_config_path}}",
      "{{/parallel}}"
    )
  )
  withr::local_options(
    list(
      slurmtools.slurm_job_template_path = template,
      slurmtools.bbi_config_path = "/opt/bbi/bbi.yaml"
    )
  )

  model_dir <- withr::local_tempdir()
  mod <- structure(
    list(absolute_model_path = file.path(model_dir, "1001")),
    class = c("bbi_nonmem_model", "bbi_base_model", "bbi_model", "list")
  )

  cmd <- submit_slurm_job(
    mod,
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE
  )

  expect_match(cmd$template_script, "nonmem run local", fixed = TRUE)
  expect_match(cmd$template_script, "1001.mod", fixed = TRUE)
  expect_match(cmd$template_script, "--parallel --threads=2", fixed = TRUE)
  expect_match(cmd$template_script, "--config /opt/bbi/bbi.yaml", fixed = TRUE)
  expect_match(cmd$template_script, "1001-nonmem-run", fixed = TRUE)
})

test_that("bbi models also render into the shipped general template via {{command}}", {
  local_submission_root()
  withr::local_options(
    list(
      slurmtools.slurm_job_template_path = system.file(
        "templates",
        "slurm-job-generic.tmpl",
        package = "slurmtools"
      ),
      slurmtools.bbi_config_path = "/opt/bbi/bbi.yaml"
    )
  )

  model_dir <- withr::local_tempdir()
  mod <- structure(
    list(absolute_model_path = file.path(model_dir, "1001")),
    class = c("bbi_nonmem_model", "bbi_base_model", "bbi_model", "list")
  )

  cmd <- submit_slurm_job(mod, partition = "cpu2mem4gb", dry_run = TRUE)

  expect_match(cmd$template_script, "nonmem run local", fixed = TRUE)
  expect_match(cmd$template_script, "--config /opt/bbi/bbi.yaml", fixed = TRUE)
})

test_that("hyperion models require the hyperion package", {
  skip_if(
    requireNamespace("hyperion", quietly = TRUE),
    "hyperion is installed; the missing-package error cannot fire"
  )
  mod <- structure(list(), class = "hyperion_nonmem_model")
  expect_error(
    submit_slurm_job(mod, partition = "cpu2mem4gb", dry_run = TRUE),
    "hyperion package must be installed"
  )
})

test_that("relative submission_root and template paths are tolerated", {
  # regression: rendering and submission each run inside withr::with_dir(), so
  # a relative path used to be re-resolved against the wrong directory (the
  # render dir / submission root) and fail. Both are absolutized up front now.
  stub_dir <- withr::local_tempdir()
  brio::write_file(
    "#!/bin/bash\necho Submitted batch job 0\n",
    file.path(stub_dir, "sbatch")
  )
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix")

  # run from a scratch working directory and refer to everything relatively
  work <- withr::local_tempdir()
  withr::local_dir(work)
  fs::dir_create("subroot")
  fs::dir_create("scripts")
  brio::write_file("print('hi')\n", "scripts/sim.R")
  brio::write_file("#!/bin/bash\n{{command}}\n", "job.tmpl")

  res <- submit_slurm_job(
    "scripts/sim.R",
    partition = "cpu2mem4gb",
    dry_run = FALSE,
    submission_root = "subroot",
    slurm_job_template_path = "job.tmpl"
  )

  expect_match(res$stdout, "Submitted batch job 0")
  expect_true(fs::file_exists(file.path("subroot", "sim.R.sh")))
})

test_that("submit_slurm_job.character validates the partition", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  expect_error(
    submit_slurm_job(script, partition = "not-a-partition", dry_run = TRUE)
  )
  expect_error(
    submit_slurm_job(script, partition = NULL, dry_run = TRUE),
    "no partition selected"
  )
})
