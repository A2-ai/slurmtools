local_submission_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(
    list(slurmtools.submission_root = root),
    .local_envir = env
  )
  root
}

local_stub_sbatch <- function(env = parent.frame()) {
  # a stub sbatch on the PATH keeps tests from submitting a real job
  stub_dir <- withr::local_tempdir(.local_envir = env)
  brio::write_file(
    "#!/bin/bash\necho Submitted batch job 0\n",
    file.path(stub_dir, "sbatch")
  )
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
  stub_dir
}

test_that("a .R path renders the shipped rscript template", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  cmd <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE)

  expect_match(cmd$template_script, "exec Rscript", fixed = TRUE)
  expect_match(cmd$template_script, script, fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --partition=cpu2mem4gb", fixed = TRUE)
  # no unset block anymore
  expect_no_match(cmd$template_script, "unset SLURM", fixed = TRUE)
  # log output is surfaced
  expect_match(cmd$template_script, "#SBATCH --output=", fixed = TRUE)
})

test_that("a .qmd path renders the shipped quarto template", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".qmd", lines = "# a report")

  cmd <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE)

  expect_match(cmd$template_script, "exec quarto render", fixed = TRUE)
  expect_match(cmd$template_script, script, fixed = TRUE)
})

test_that("paths are passed through untouched (no absolute-path massaging)", {
  local_submission_root()
  work <- withr::local_tempdir()
  withr::local_dir(work)
  fs::dir_create("scripts")
  brio::write_file("print('hi')\n", "scripts/sim.R")

  cmd <- submit_slurm_job("scripts/sim.R", partition = "cpu2mem4gb", dry_run = TRUE)

  # the relative path the caller gave is what lands in the command
  expect_match(cmd$template_script, "exec Rscript scripts/sim.R", fixed = TRUE)
})

test_that("a character vector submits one job per path", {
  local_submission_root()
  a <- withr::local_tempfile(fileext = ".R", lines = "1")
  b <- withr::local_tempfile(fileext = ".qmd", lines = "# b")

  res <- submit_slurm_job(c(a, b), partition = "cpu2mem4gb", dry_run = TRUE)

  expect_length(res, 2)
  expect_match(res[[1]]$template_script, "Rscript", fixed = TRUE)
  expect_match(res[[2]]$template_script, "quarto render", fixed = TRUE)
})

test_that("the worker writes and chmods the job script when not a dry run", {
  local_stub_sbatch()
  root <- local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = FALSE)

  expect_match(res$stdout, "Submitted batch job 0")
  script_file <- file.path(root, sprintf("%s.sh", basename(script)))
  expect_true(fs::file_exists(script_file))
  expect_true(fs::file_access(script_file, mode = "execute"))
})

test_that("relative submission_root and template are tolerated (no dir switch)", {
  local_stub_sbatch()
  work <- withr::local_tempdir()
  withr::local_dir(work)
  fs::dir_create("subroot")
  fs::dir_create("scripts")
  brio::write_file("print('hi')\n", "scripts/sim.R")
  brio::write_file("#!/bin/bash\nexec Rscript {{script_path}}\n", "job.tmpl")

  res <- submit_slurm_job(
    "scripts/sim.R",
    partition = "cpu2mem4gb",
    dry_run = FALSE,
    submission_root = "subroot",
    template = "job.tmpl"
  )

  expect_match(res$stdout, "Submitted batch job 0")
  expect_true(fs::file_exists(file.path("subroot", "sim.R.sh")))
})

test_that("bare NONMEM control streams are refused, even with a template", {
  local_submission_root()
  model <- withr::local_tempfile(fileext = ".mod", lines = "$PROBLEM test")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c("#!/bin/bash", "my-tool {{script_path}}")
  )

  expect_error(
    submit_slurm_job(model, partition = "cpu2mem4gb", dry_run = TRUE),
    "not a submittable object"
  )
  # item 6: a bare .ctl errors regardless of a supplied template
  expect_error(
    submit_slurm_job(
      model,
      partition = "cpu2mem4gb",
      dry_run = TRUE,
      template = template
    ),
    "not a submittable object"
  )
})

test_that("unsupported extensions error without a user template", {
  local_submission_root()
  file <- withr::local_tempfile(fileext = ".txt", lines = "hello")

  expect_error(
    submit_slurm_job(file, partition = "cpu2mem4gb", dry_run = TRUE),
    "don't know how to submit a `.txt` file"
  )
})

test_that("a user-supplied template accepts any file type", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".py", lines = "print('hi')")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c(
      "#!/bin/bash",
      "#SBATCH --partition={{partition}}",
      "source activate {{conda_env}}",
      "python {{script_path}}"
    )
  )

  cmd <- submit_slurm_job(
    script,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    template = template,
    template_opts = list(conda_env = "ml")
  )

  expect_match(cmd$template_script, "source activate ml", fixed = TRUE)
  expect_match(cmd$template_script, sprintf("python %s", script), fixed = TRUE)
})

test_that("template_opts override the values a method builds", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c("#!/bin/bash", "job={{job_name}}")
  )

  cmd <- submit_slurm_job(
    script,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    template = template,
    template_opts = list(job_name = "custom-name")
  )

  expect_match(cmd$template_script, "job=custom-name", fixed = TRUE)
})

test_that("errors on missing files", {
  local_submission_root()
  expect_error(
    submit_slurm_job("does-not-exist.R", dry_run = TRUE),
    "no such file"
  )
})

test_that("submit_slurm_job.default rejects unknown classes", {
  expect_error(
    submit_slurm_job(42),
    "don't know how to submit an object of class <numeric>"
  )
})

test_that("a bbi model renders the shipped bbi template", {
  local_submission_root()
  model_dir <- withr::local_tempdir()
  mod <- structure(
    list(absolute_model_path = file.path(model_dir, "1001")),
    class = c("bbi_nonmem_model", "bbi_base_model", "bbi_model", "list")
  )

  cmd <- submit_slurm_job(
    mod,
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE,
    config_path = "/opt/bbi/bbi.yaml"
  )

  expect_match(cmd$template_script, "bbi nonmem run local", fixed = TRUE)
  expect_match(cmd$template_script, "1001.mod", fixed = TRUE)
  expect_match(cmd$template_script, "--parallel --threads=2", fixed = TRUE)
  expect_match(cmd$template_script, "--config /opt/bbi/bbi.yaml", fixed = TRUE)
  expect_match(cmd$template_script, "1001-nonmem-run", fixed = TRUE)
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

test_that("partition is validated", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  expect_error(
    submit_slurm_job(script, partition = "not-a-partition", dry_run = TRUE),
    "not an available partition"
  )
  expect_error(
    submit_slurm_job(script, partition = NULL, dry_run = TRUE),
    "no partition selected"
  )
})
