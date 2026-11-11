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
    "#!/bin/bash\necho 0\n", # what sbatch --parsable prints
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
  # as the package will see it: Sys.which() follows macOS's /var -> /private/var
  # symlink and tidies the `//` tempdir() can leave, so the literal path would not match
  unname(Sys.which(name))
}

# the partition table, so no test here needs a real sinfo (deep-review finding 4)
local_partition_table()

# --- create_slurm_template() ------------------------------------------------

test_that("create_slurm_template writes default_template()'s header, --output, and the resolved command", {
  rscript <- local_stub_bin("Rscript")
  path <- file.path(withr::local_tempdir(), "rscript.tmpl")

  expect_message(
    create_slurm_template(path, command = "Rscript", args = "{{file}}"),
    "wrote"
  )

  content <- brio::read_file(path)
  # the command names the absolute binary resolved at creation time
  expect_match(content, sprintf("\n%s {{file}}", rscript), fixed = TRUE)
  expect_match(content, "#SBATCH --job-name={{job_name}}", fixed = TRUE)
  expect_match(content, "#SBATCH --cpus-per-task={{ncpu}}", fixed = TRUE)
  expect_match(content, "{{#account}}\n#SBATCH --account={{account}}\n{{/account}}", fixed = TRUE)
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )

  expect_match(cmd$template_script, script, fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --partition=cpu2mem4gb", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --output=", fixed = TRUE)
  expect_match(
    cmd$template_script,
    sprintf("--job-name=%s", script_stem(script)),
    fixed = TRUE
  )
})

test_that("slurm_job_template_path is required, with a pointer to the scaffolder", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  withr::local_options(slurmtools.slurm_job_template_path = NULL)
  expect_error(
    submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE),
    "create_slurm_template"
  )
  expect_error(
    submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE),
    "`slurm_job_template_path` is required"
  )
})

test_that("the template option serves the file form too", {
  local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  withr::local_options(slurmtools.slurm_job_template_path = template)
  cmd <- submit_slurm_job(script, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_match(cmd$template_script, script, fixed = TRUE)
})

test_that("bbi_config_path is a value the file form supplies, from the argument or the option", {
  local_submission_root()
  script <- withr::local_tempfile(fileext = ".mod", lines = "$PROBLEM test")
  template <- withr::local_tempfile(fileext = ".tmpl", lines = c("#!/bin/bash", "bbi run {{file}} --config {{bbi_config_path}}"))
  cmd <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", bbi_config_path = "cfg/bbi.yaml", dry_run = TRUE)
  expect_match(cmd$template_script, "--config cfg/bbi.yaml", fixed = TRUE)
  withr::local_options(slurmtools.bbi_config_path = "opt/bbi.yaml")
  cmd <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_match(cmd$template_script, "--config opt/bbi.yaml", fixed = TRUE)
})

test_that("overwrite and an unnamed slurm_template_opts are refused in the file form", {
  local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  expect_error(
    submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", overwrite = TRUE, dry_run = TRUE),
    "`overwrite` belongs to the pre-template NONMEM form"
  )
  expect_error(
    submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", dry_run = TRUE, slurm_template_opts = list("x")),
    "must be a named list"
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )

  # the relative path the caller gave is what lands in the command
  expect_match(cmd$template_script, " scripts/sim.R", fixed = TRUE)
})

test_that("slurm_template_opts supply extra variables and override built-in values", {
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    slurm_template_opts = list(conda_env = "ml", job_name = "custom-name")
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = TRUE
  )
  expect_no_match(serial$template_script, "--parallel", fixed = TRUE)

  parallel <- submit_slurm_job(
    model,
    slurm_job_template_path = template,
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = FALSE
  )

  expect_equal(res$job_id, "0")
  script_file <- file.path(root, sprintf("%s-0.sh", script_stem(script)))
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
    slurm_job_template_path = template,
    partition = "cpu2mem4gb",
    dry_run = FALSE
  )

  expect_equal(res$job_id, "0")
  expect_true(fs::file_exists(file.path("submission-log", "sim-0.sh")))
})

test_that("errors on missing files", {
  local_submission_root()
  template <- local_rscript_template()

  expect_error(
    submit_slurm_job("does-not-exist.R", slurm_job_template_path = template, dry_run = TRUE),
    "no such file"
  )
})

test_that("non-path inputs are rejected", {
  local_submission_root()
  template <- local_rscript_template()

  expect_error(
    submit_slurm_job(42, slurm_job_template_path = template, dry_run = TRUE),
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
      slurm_job_template_path = template,
      partition = "not-a-partition",
      dry_run = TRUE
    ),
    "not an available partition"
  )
  expect_error(
    submit_slurm_job(script, slurm_job_template_path = template, partition = NULL, dry_run = TRUE),
    "no partition selected"
  )
})

test_that("the file form returns the same record as a Template submit, and the follow-up functions take it", {
  # deep-review finding 7: sbatch --parsable on every route, one return shape
  local_stub_sbatch()
  root <- local_submission_root()
  template <- local_rscript_template() # writes --output={{log_path}}
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb")
  expect_equal(
    names(res),
    c("status", "stdout", "stderr", "timeout", "job_id", "job_name", "partition", "script", "output", "error")
  )
  expect_equal(res$job_id, "0")
  expect_equal(res$job_name, script_stem(script))
  expect_equal(res$partition, "cpu2mem4gb")
  expect_equal(res$script, file.path(root, sprintf("%s-0.sh", script_stem(script))))
  expect_equal(res$output, file.path(root, sprintf("%s-0.out", script_stem(script))))
  # the scaffold's --error line, {{log_dir}}/{{job_name}}-%j.err, resolves the same way
  expect_equal(res$error, file.path(root, sprintf("%s-0.err", script_stem(script))))
  expect_match(brio::read_file(res$script), sprintf("--output=%s/%s-%%j.out", root, script_stem(script)), fixed = TRUE)
  expect_match(brio::read_file(res$script), sprintf("--error=%s/%s-%%j.err", root, script_stem(script)), fixed = TRUE)

  calls <- stub_sacct_sequence(sacct_row("COMPLETED", id = "0"))
  expect_equal(suppressMessages(wait_for_slurm_job(res, poll = 0.01))$state, "COMPLETED")
  expect_equal(slurm_job_status(res)$job_id, "0")
})

test_that("a file-form template that leaves --output to slurm gives an NA log path, not a wrong one", {
  local_stub_sbatch()
  root <- local_submission_root()
  template <- file.path(withr::local_tempdir(), "bare.tmpl")
  writeLines(c("#!/bin/bash", "#SBATCH --partition={{partition}}", "echo {{file}}"), template)
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb")
  expect_equal(res$job_id, "0")
  expect_true(is.na(res$output))
  expect_error(slurm_job_log(res), "left to slurm")
})

# --- one script per job, nothing left by a refused submit (deep-review finding 9, decision 9) ---

test_that("each submit keeps its own script, named like its logs, and the plain name is never left behind", {
  stub_dir <- withr::local_tempdir()
  counter <- file.path(stub_dir, "n")
  writeLines("100", counter)
  # an sbatch that hands out 101, 102, ... and records the script it was given
  brio::write_file(
    sprintf("#!/bin/bash\nn=$(( $(cat '%s') + 1 ))\necho $n > '%s'\ncp \"$2\" '%s/given-'$n\necho $n\n", counter, counter, stub_dir),
    file.path(stub_dir, "sbatch")
  )
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix")
  root <- local_submission_root()
  # a template whose rendered text differs per submit, so the kept scripts must too
  template <- file.path(withr::local_tempdir(), "seeded.tmpl")
  writeLines(c("#!/bin/bash", "#SBATCH --job-name={{job_name}}", "#SBATCH --partition={{partition}}", "echo {{file}} --seed={{seed}}"), template)
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  first <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb",
                            slurm_template_opts = list(seed = 1))
  second <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb",
                             slurm_template_opts = list(seed = 2))
  name <- script_stem(script)
  expect_equal(first$script, file.path(root, sprintf("%s-101.sh", name)))
  expect_equal(second$script, file.path(root, sprintf("%s-102.sh", name)))
  expect_true(fs::file_exists(first$script))
  expect_true(fs::file_exists(second$script))
  # each kept script is byte for byte what sbatch was handed for that job
  expect_equal(brio::read_file(first$script), brio::read_file(file.path(stub_dir, "given-101")))
  expect_equal(brio::read_file(second$script), brio::read_file(file.path(stub_dir, "given-102")))
  expect_match(brio::read_file(first$script), "--seed=1\n", fixed = TRUE)
  expect_match(brio::read_file(second$script), "--seed=2\n", fixed = TRUE)
  expect_false(fs::file_exists(file.path(root, sprintf("%s.sh", name))))
  expect_length(fs::dir_ls(root, glob = "*.tmp"), 0)
})

test_that("a submit sbatch refuses leaves no script behind", {
  stub_dir <- withr::local_tempdir()
  brio::write_file("#!/bin/bash\necho 'sbatch: error: Invalid directive found in batch script: model' >&2\nexit 1\n", file.path(stub_dir, "sbatch"))
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix")
  root <- local_submission_root()
  template <- local_rscript_template()
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  expect_error(
    submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb"),
    "Invalid directive found in batch script: model"
  )
  expect_length(fs::dir_ls(root), 0)
})

test_that("a Template submit logs every value it fills, defaults and derived ones included", {
  withr::local_envvar(SLURMTOOLS_VERBOSE = "INFO")
  toggle_logger(quiet = TRUE)
  withr::defer({
    Sys.unsetenv("SLURMTOOLS_VERBOSE")
    toggle_logger(quiet = TRUE)
  })
  local_submission_root()
  tmpl <- Template() |> with_job_name() |> with_partition() |> with_cpus_per_task("{{ncpu}}") |> with_command("echo", "{{file}}")
  out <- capture.output(submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R")))
  expect_match(paste(out, collapse = "\n"), "filling template with")
  expect_match(paste(out, collapse = "\n"), "partition: cpu2mem4gb")
  expect_match(paste(out, collapse = "\n"), "ncpu: 1")
  expect_match(paste(out, collapse = "\n"), "job_name: sim")
  # derived values too, not only what was given
  expect_match(paste(out, collapse = "\n"), "file_stem: sim")
  expect_match(paste(out, collapse = "\n"), "parallel: FALSE")
})

test_that("the file form's log paths come from the rendered #SBATCH lines, not from a mention of log_path", {
  local_stub_sbatch()
  root <- local_submission_root()
  template <- file.path(withr::local_tempdir(), "mention.tmpl")
  writeLines(c(
    "#!/bin/bash",
    "#SBATCH --partition={{partition}}",
    "#SBATCH --error=\"{{log_path}}.err\"",
    "# the output would go to {{log_path}}, but no --output line asks for it",
    "echo {{file}}"
  ), template)
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  res <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb")
  expect_true(is.na(res$output))
  expect_equal(res$error, file.path(root, sprintf("%s-0.out.err", script_stem(script))))
})

test_that("an #SBATCH line after the first command is a comment to Slurm, and to the result", {
  local_stub_sbatch()
  root <- local_submission_root()
  template <- file.path(withr::local_tempdir(), "late.tmpl")
  writeLines(c(
    "#!/bin/bash",
    "#SBATCH --partition={{partition}}",
    "echo {{file}}",
    "#SBATCH --output=wrong.out"
  ), template)
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  res <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb")
  expect_true(is.na(res$output))
})

test_that("a template that names its job with Slurm's short form -J is not overridden", {
  local_submission_root()
  template <- file.path(withr::local_tempdir(), "short.tmpl")
  writeLines(c("#!/bin/bash", "#SBATCH -J chosen", "#SBATCH --partition={{partition}}", "echo {{file}}"), template)
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")
  cmd <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_false(any(grepl("^--job-name", cmd$args)))
  # and one with no name at all gets the script's stem
  writeLines(c("#!/bin/bash", "#SBATCH --partition={{partition}}", "echo {{file}}"), template)
  cmd <- submit_slurm_job(script, slurm_job_template_path = template, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_true(sprintf("--job-name=%s", script_stem(script)) %in% cmd$args)
})
