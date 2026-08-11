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

local_stub_bin <- function(name, env = parent.frame()) {
  # a stub tool on the PATH so Sys.which() resolution is deterministic and
  # works on machines (like CI) where the real tool is not installed
  stub_dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(stub_dir, name)
  brio::write_file("#!/bin/bash\n", path)
  fs::file_chmod(path, "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
  path
}

# --- create_slurm_template() ------------------------------------------------

test_that("create_slurm_template writes a header plus the resolved command", {
  rscript <- local_stub_bin("Rscript")
  path <- file.path(withr::local_tempdir(), "rscript.tmpl")

  expect_message(
    create_slurm_template(path, command = "Rscript", args = "{{file}}"),
    "wrote"
  )

  content <- brio::read_file(path)
  # the command names the absolute binary resolved at creation time
  expect_match(content, sprintf("\n%s {{file}}", rscript), fixed = TRUE)
  expect_match(content, "#SBATCH --job-name=\"{{job_name}}\"", fixed = TRUE)
  expect_match(content, "#SBATCH --partition={{partition}}", fixed = TRUE)
  expect_match(content, "#SBATCH --output={{log_path}}", fixed = TRUE)
  # no parallel variant unless asked for
  expect_no_match(content, "{{#parallel}}", fixed = TRUE)
})

test_that("parallel_args add a parallel variant of the command", {
  bbi <- local_stub_bin("bbi")
  path <- file.path(withr::local_tempdir(), "bbi.tmpl")

  suppressMessages(create_slurm_template(
    path,
    command = "bbi",
    args = c("nonmem", "run", "local", "{{file}}", "--config", "{{config_path}}"),
    parallel_args = c("--parallel", "--threads={{ncpu}}")
  ))

  content <- brio::read_file(path)
  expect_match(
    content,
    sprintf(
      "{{#parallel}}\n%s nonmem run local {{file}} --config {{config_path}} --parallel --threads={{ncpu}}\n{{/parallel}}",
      bbi
    ),
    fixed = TRUE
  )
  expect_match(
    content,
    sprintf(
      "{{^parallel}}\n%s nonmem run local {{file}} --config {{config_path}}\n{{/parallel}}",
      bbi
    ),
    fixed = TRUE
  )
})

test_that("a command containing a slash skips PATH resolution", {
  path <- file.path(withr::local_tempdir(), "custom.tmpl")

  suppressMessages(create_slurm_template(
    path,
    command = "/opt/custom/bbi",
    args = "{{file}}"
  ))

  expect_match(
    brio::read_file(path),
    "\n/opt/custom/bbi {{file}}",
    fixed = TRUE
  )
})

test_that("create_slurm_template fails fast when the command is not on the PATH", {
  path <- file.path(withr::local_tempdir(), "missing.tmpl")
  empty_dir <- withr::local_tempdir()
  withr::local_envvar(c(PATH = empty_dir))

  expect_error(
    create_slurm_template(path, command = "no-such-tool", args = "{{file}}"),
    "could not find `no-such-tool` on the PATH"
  )
  expect_false(fs::file_exists(path))
})

test_that("create_slurm_template refuses to overwrite unless asked", {
  local_stub_bin("Rscript")
  path <- file.path(withr::local_tempdir(), "rscript.tmpl")

  suppressMessages(create_slurm_template(path, command = "Rscript"))
  expect_error(
    create_slurm_template(path, command = "Rscript"),
    "already exists"
  )
  expect_no_error(suppressMessages(
    create_slurm_template(path, command = "Rscript", overwrite = TRUE)
  ))
})

# --- submit_slurm_job() -----------------------------------------------------

local_rscript_template <- function(env = parent.frame()) {
  local_stub_bin("Rscript", env = env)
  path <- file.path(withr::local_tempdir(.local_envir = env), "rscript.tmpl")
  suppressMessages(create_slurm_template(path, command = "Rscript", args = "{{file}}"))
  path
}

test_that("submitting renders the template with the file and slurm values", {
  local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  cmd <- submit_slurm_job(
    script,
    template = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )

  expect_match(cmd$template_script, script, fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --partition=cpu2mem4gb", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --output=", fixed = TRUE)
  expect_match(
    cmd$template_script,
    sprintf("--job-name=\"%s\"", basename(script)),
    fixed = TRUE
  )
})

test_that("template is required, with a pointer to the scaffolder", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  expect_error(
    submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE),
    "create_slurm_template"
  )
})

test_that("paths are passed through untouched (no absolute-path massaging)", {
  local_submission_root()
  template <- local_rscript_template()
  work <- withr::local_tempdir()
  withr::local_dir(work)
  fs::dir_create("scripts")
  brio::write_file("print('hi')\n", "scripts/sim.R")

  cmd <- submit_slurm_job(
    "scripts/sim.R",
    template = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )

  # the relative path the caller gave is what lands in the command
  expect_match(cmd$template_script, " scripts/sim.R", fixed = TRUE)
})

test_that("template_opts supply extra variables and override built-in values", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".py", lines = "print('hi')")
  template <- withr::local_tempfile(
    fileext = ".tmpl",
    lines = c(
      "#!/bin/bash",
      "#SBATCH --job-name=\"{{job_name}}\"",
      "source activate {{conda_env}}",
      "python {{file}}"
    )
  )

  cmd <- submit_slurm_job(
    script,
    template = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    template_opts = list(conda_env = "ml", job_name = "custom-name")
  )

  expect_match(cmd$template_script, "source activate ml", fixed = TRUE)
  expect_match(cmd$template_script, sprintf("python %s", script), fixed = TRUE)
  expect_match(cmd$template_script, "--job-name=\"custom-name\"", fixed = TRUE)
})

test_that("ncpu > 1 flips the template's parallel block on", {
  local_submission_root()
  bbi <- local_stub_bin("bbi")
  template <- file.path(withr::local_tempdir(), "bbi.tmpl")
  suppressMessages(create_slurm_template(
    template,
    command = "bbi",
    args = c("nonmem", "run", "local", "{{file}}"),
    parallel_args = c("--parallel", "--threads={{ncpu}}")
  ))
  model <- withr::local_tempfile(fileext = ".mod", lines = "$PROBLEM test")

  serial <- submit_slurm_job(
    model,
    template = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )
  expect_no_match(serial$template_script, "--parallel", fixed = TRUE)

  parallel <- submit_slurm_job(
    model,
    template = template,
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE
  )
  expect_match(
    parallel$template_script,
    "--parallel --threads=2",
    fixed = TRUE
  )
})

test_that("the worker writes and chmods the job script when not a dry run", {
  local_stub_sbatch()
  root <- local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(
    script,
    template = template,
    partition = "cpu2mem4gb",
    dry_run = FALSE
  )

  expect_match(res$stdout, "Submitted batch job 0")
  script_file <- file.path(root, sprintf("%s.sh", basename(script)))
  expect_true(fs::file_exists(script_file))
  expect_true(fs::file_access(script_file, mode = "execute"))
})

test_that("submission_root defaults to submission-log under the calling directory", {
  local_stub_sbatch()
  template <- local_rscript_template()
  work <- withr::local_tempdir()
  withr::local_dir(work)
  withr::local_options(list(slurmtools.submission_root = NULL))
  brio::write_file("print('hi')\n", "sim.R")

  res <- submit_slurm_job(
    "sim.R",
    template = template,
    partition = "cpu2mem4gb",
    dry_run = FALSE
  )

  expect_match(res$stdout, "Submitted batch job 0")
  expect_true(fs::file_exists(file.path("submission-log", "sim.R.sh")))
})

test_that("errors on missing files", {
  local_submission_root()
  template <- local_rscript_template()

  expect_error(
    submit_slurm_job("does-not-exist.R", template = template, dry_run = TRUE),
    "no such file"
  )
})

test_that("non-path inputs are rejected", {
  local_submission_root()
  template <- local_rscript_template()

  expect_error(
    submit_slurm_job(42, template = template, dry_run = TRUE),
    "must be a single path"
  )
})

test_that("partition is validated", {
  local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  expect_error(
    submit_slurm_job(
      script,
      template = template,
      partition = "not-a-partition",
      dry_run = TRUE
    ),
    "not an available partition"
  )
  expect_error(
    submit_slurm_job(script, template = template, partition = NULL, dry_run = TRUE),
    "no partition selected"
  )
})
