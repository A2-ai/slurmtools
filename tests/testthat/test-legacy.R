# the pre-template interface (main at e4b747d) routed through submit_slurm_job()

# main's vignette template (vignettes/model/nonmem/slurm-job-bbi.tmpl), minus the
# unset block: every variable main's submit_slurm_job() supplied
legacy_template <- function(dir) {
  path <- file.path(dir, "slurm-job-bbi.tmpl")
  writeLines(c(
    "#!/bin/bash",
    "#SBATCH --job-name=\"{{job_name}}\"",
    "#SBATCH --cpus-per-task={{ncpu}}",
    "#SBATCH --partition={{partition}}",
    "#SBATCH --account={{project_name}}",
    "#{{project_path}}",
    "{{#parallel}}",
    "{{bbi_exe_path}} nonmem run local {{model_path}}.mod --parallel --threads={{ncpu}} --config {{bbi_config_path}}",
    "{{/parallel}}",
    "{{^parallel}}",
    "{{bbi_exe_path}} nonmem run local {{model_path}}.mod --config {{bbi_config_path}}",
    "{{/parallel}}"
  ), path)
  path
}

# a stub bbi on the PATH (test-submit-slurm-job.R keeps the general local_stub_bin())
local_stub_bbi <- function(env = parent.frame()) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(stub_dir, "bbi")
  brio::write_file("#!/bin/bash\n", path)
  fs::file_chmod(path, "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
  path
}

# a model file, a stub bbi, main's template, a fake partition table, and the
# legacy warning shown on every call rather than once per session
local_legacy <- function(env = parent.frame()) {
  local_partition_table(env = env)
  withr::local_options(rlib_warning_verbosity = "verbose", .local_envir = env)
  dir <- withr::local_tempdir(.local_envir = env)
  mod <- file.path(dir, "1001.mod")
  writeLines("$PROBLEM", mod)
  root <- file.path(dir, "submission-log")
  withr::local_options(slurmtools.submission_root = root, .local_envir = env)
  list(
    dir = dir,
    mod = mod,
    model_path = as.character(fs::path_abs(tools::file_path_sans_ext(mod))),
    bbi = local_stub_bbi(env = env),
    template = legacy_template(dir),
    root = root
  )
}

a_bbr_model <- function(model_path) {
  structure(list(absolute_model_path = model_path), class = c("bbi_nonmem_model", "list"))
}

opts <- list(project_path = "/proj", project_name = "proj")

test_that("main's vignette call (options + a model path) runs the old code", {
  l <- local_legacy()
  withr::local_options(
    slurmtools.slurm_job_template_path = l$template,
    slurmtools.bbi_config_path = "/cfg/bbi.yaml"
  )
  expect_warning(
    cmd <- submit_slurm_job(l$mod, partition = "cpu2mem4gb", ncpu = 2, dry_run = TRUE),
    "pre-template way"
  )
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=\"1001-nonmem-run\"" %in% script)
  expect_true("#SBATCH --cpus-per-task=2" %in% script)
  expect_match(cmd$template_script, "#SBATCH --account=\\S+") # here::here()'s basename
  expect_true(sprintf(
    "%s nonmem run local %s.mod --parallel --threads=2 --config /cfg/bbi.yaml", l$bbi, l$model_path
  ) %in% script)
  expect_equal(cmd$args, file.path(l$root, "1001.sh"))
  expect_equal(cmd$partition, "cpu2mem4gb")
})

test_that("a bbr model object is routed to the old code", {
  l <- local_legacy()
  expect_warning(
    cmd <- submit_slurm_job(
      a_bbr_model(l$model_path),
      partition = "cpu2mem4gb",
      dry_run = TRUE,
      slurm_job_template_path = l$template,
      bbi_config_path = "/cfg/bbi.yaml",
      slurm_template_opts = opts
    ),
    "pre-template way"
  )
  script <- strsplit(cmd$template_script, "\n")[[1]]
  expect_true(sprintf("%s nonmem run local %s.mod --config /cfg/bbi.yaml", l$bbi, l$model_path) %in% script)
  expect_true("#SBATCH --account=proj" %in% script)
  expect_true("#/proj" %in% script)
})

test_that("`.mod =` is routed to the old code", {
  l <- local_legacy()
  expect_warning(
    cmd <- submit_slurm_job(
      .mod = l$mod, partition = "cpu2mem4gb", dry_run = TRUE,
      slurm_job_template_path = l$template, slurm_template_opts = opts
    ),
    "pre-template way"
  )
  expect_match(cmd$template_script, "--job-name=\"1001-nonmem-run\"", fixed = TRUE)
})

test_that("main's positional order (.mod, partition, ncpu, overwrite, dry_run) binds as it did", {
  l <- local_legacy()
  expect_warning(
    cmd <- submit_slurm_job(
      a_bbr_model(l$model_path), "cpu4mem32gb", 4, FALSE, TRUE,
      slurm_job_template_path = l$template, slurm_template_opts = opts
    ),
    "pre-template way"
  )
  expect_equal(cmd$partition, "cpu4mem32gb")
  expect_match(cmd$template_script, "#SBATCH --cpus-per-task=4", fixed = TRUE)
  expect_match(cmd$template_script, "--parallel --threads=4", fixed = TRUE)
})

test_that("overwrite = TRUE deletes the model's run directory, as main did", {
  l <- local_legacy()
  fs::dir_create(l$model_path)
  expect_warning(
    submit_slurm_job(
      l$mod, partition = "cpu2mem4gb", overwrite = TRUE, dry_run = TRUE,
      slurm_job_template_path = l$template, slurm_template_opts = opts
    ),
    "pre-template way"
  )
  expect_false(fs::dir_exists(l$model_path))
})

test_that("the warning points at the template way", {
  l <- local_legacy()
  expect_warning(
    submit_slurm_job(l$mod, partition = "cpu2mem4gb", dry_run = TRUE,
                     slurm_job_template_path = l$template, slurm_template_opts = opts),
    "default_template(\"bbi\"",
    fixed = TRUE
  )
})

test_that("template-form and file-form calls are not routed to the old code", {
  l <- local_legacy()
  withr::local_options(slurmtools.slurm_job_template_path = l$template)
  tmpl <- default_template("echo", "{{file}}")

  # a Template is never legacy, even with the old option still set
  expect_no_warning(submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE, slurm_template_opts = list(file = "a.R")))
  # nor is a model path with a template written for the file form: what
  # decides is the template, not the argument it came through
  file_tmpl <- file.path(l$dir, "new.tmpl")
  writeLines(c("#!/bin/bash", "echo {{file}}"), file_tmpl)
  expect_no_warning(cmd <- submit_slurm_job(l$mod, slurm_job_template_path = file_tmpl, partition = "cpu2mem4gb", dry_run = TRUE))
  expect_match(cmd$template_script, paste("echo", l$mod), fixed = TRUE)
  withr::local_options(slurmtools.slurm_job_template_path = file_tmpl)
  expect_no_warning(submit_slurm_job(l$mod, partition = "cpu2mem4gb", dry_run = TRUE))
})

test_that("what routes to the old code is a bbr model, or a template that takes {{model_path}}", {
  l <- local_legacy()
  expect_true(is_legacy_template(l$template))
  new_tmpl <- withr::local_tempfile(fileext = ".tmpl", lines = c("#!/bin/bash", "echo {{file}}"))
  expect_false(is_legacy_template(new_tmpl))
  expect_false(is_legacy_template(NULL))
  expect_false(is_legacy_template("no-such-file.tmpl"))
  expect_true(is_legacy_submit(a_bbr_model(l$model_path), new_tmpl))
  expect_true(is_legacy_submit(l$mod, l$template))
  expect_false(is_legacy_submit(l$mod, new_tmpl))
})

test_that("a path with no template and no old option still asks for a template", {
  l <- local_legacy()
  withr::local_options(slurmtools.slurm_job_template_path = NULL)
  expect_error(submit_slurm_job(l$mod, partition = "cpu2mem4gb", dry_run = TRUE), "`slurm_job_template_path` is required")
})
