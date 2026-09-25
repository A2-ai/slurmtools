# --- derived builtins: file_dir / file_stem / file_ext / log_dir ----------------

rscript <- "/opt/R/4.5.3/bin/Rscript"
ln <- unname(Sys.which("ln"))
echo <- unname(Sys.which("echo"))
cp <- unname(Sys.which("cp"))

# the corpus ex. 8 recipe: a flat symlink to the .lst NONMEM writes into its own directory
nonmem <- function() {
  Template() |>
    with_job_name() |>
    with_command("/opt/bbi/bbi", c("nonmem", "run", "local", "{{file}}")) |>
    with_post_run("ln", c("-sf", "{{file_dir}}/{{file_stem}}/{{file_stem}}.lst", "{{file_dir}}/{{file_stem}}.lst"))
}

test_that("the pieces are derived with fs semantics, and never by partial matching", {
  expect_equal(
    derived_fills(list(file = "model/nonmem/1001.mod")),
    list(file_dir = "model/nonmem", file_stem = "1001", file_ext = "mod")
  )
  expect_equal(derived_fills(list(file = "sim.R")), list(file_dir = ".", file_stem = "sim", file_ext = "R"))
  expect_equal(derived_fills(list(file = "/abs/path/run"))$file_ext, "")
  expect_equal(derived_fills(list(file = "a/b.tar.gz"))$file_stem, "b.tar")
  expect_equal(derived_fills(list(file = "sim.R"), submission_root = "logs")$log_dir, "logs")
  expect_equal(derived_fills(list(), submission_root = "logs"), list(log_dir = "logs"))
  expect_length(derived_fills(list()), 0)
  expect_length(derived_fills(list(file_stem = "x")), 0)
})

test_that("file_dir / file_stem / file_ext follow the file fill in format() and print()", {
  tmpl <- nonmem()
  expect_true(
    paste(ln, "-sf {{file_dir}}/{{file_stem}}/{{file_stem}}.lst {{file_dir}}/{{file_stem}}.lst") %in% format(tmpl)
  )
  filled <- fill(tmpl, file = "model/nonmem/1001.mod")
  expect_true(paste(ln, "-sf model/nonmem/1001/1001.lst model/nonmem/1001.lst") %in% format(filled))
  expect_true("/opt/bbi/bbi nonmem run local model/nonmem/1001.mod" %in% format(filled))
  expect_output(print(filled), "-sf model/nonmem/1001/1001.lst", fixed = TRUE)
})

test_that("a template that writes only a derived name still takes `file`", {
  tmpl <- Template() |> with_job_name() |> with_command("/opt/bbi/bbi", "{{file_stem}}")
  expect_setequal(template_placeholders(tmpl), c("job_name", "file_stem", "file"))
  expect_equal(utils::tail(format(fill(tmpl, file = "model/1001.mod")), 1), "/opt/bbi/bbi 1001")
  expect_error(fill(tmpl, fil = "x"), "`file`", fixed = TRUE)

  # no derived name written: `file` is not a placeholder, as before
  plain <- Template() |> with_job_name() |> with_command(rscript, "{{script}}")
  expect_error(fill(plain, file = "x"), "no placeholder `{{file}}`", fixed = TRUE)
})

test_that("an explicit fill of a builtin wins over the derived value", {
  filled <- fill(nonmem(), file = "model/nonmem/1001.mod", file_stem = "run1001")
  expect_true(paste(ln, "-sf model/nonmem/run1001/run1001.lst model/nonmem/run1001.lst") %in% format(filled))
})

test_that("the corpus ex. 8 symlink submits with only `file` given", {
  dry <- submit_slurm_job(nonmem(), dry_run = TRUE, slurm_template_opts = list(file = "model/nonmem/1001.mod", job_name = "1001"))
  script <- strsplit(dry$template_script, "\n")[[1]]
  expect_equal(utils::tail(script, 1), paste(ln, "-sf model/nonmem/1001/1001.lst model/nonmem/1001.lst"))
  expect_false(any(grepl("{{", script, fixed = TRUE)))
})

test_that("a derived name written without `file` is reported as needing `file`", {
  tmpl <- Template() |> with_job_name() |> with_command("/opt/bbi/bbi", "{{file_stem}}")
  expect_error(
    submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "x")),
    "unfilled placeholders: `{{file_stem}}`",
    fixed = TRUE
  )
  expect_error(
    submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "x")),
    "submit_slurm_job(template, slurm_template_opts = list(file = ...))",
    fixed = TRUE
  )
  expect_error(submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "x")), "derive from `file`", fixed = TRUE)
})

test_that("log_dir is a tag until submit, then the submission root; an explicit fill wins", {
  tmpl <- Template() |>
    with_job_name() |>
    with_command(rscript, "{{file}}") |>
    with_post_run("cp", c("Rplots.pdf", "{{log_dir}}/"))
  expect_true(paste(cp, "Rplots.pdf {{log_dir}}/") %in% format(fill(tmpl, file = "sim.R")))

  path <- withr::local_tempfile(fileext = ".tmpl")
  suppressMessages(write_slurm_template(tmpl, path))
  expect_true(paste(cp, "Rplots.pdf {{log_dir}}/") %in% readLines(path))

  dry <- submit_slurm_job(tmpl, submission_root = "logs/here", dry_run = TRUE, slurm_template_opts = list(file = "sim.R", job_name = "sim"))
  expect_match(dry$template_script, paste(cp, "Rplots.pdf logs/here/"), fixed = TRUE)
  expect_match(dry$template_script, "--output=logs/here/sim-%j.out", fixed = TRUE)

  dry <- submit_slurm_job(
    tmpl, submission_root = "logs/here", dry_run = TRUE,
    slurm_template_opts = list(file = "sim.R", job_name = "sim", log_dir = "elsewhere")
  )
  expect_match(dry$template_script, paste(cp, "Rplots.pdf elsewhere/"), fixed = TRUE)
})

test_that("the file form gets the same builtins", {
  path <- withr::local_tempfile(fileext = ".tmpl")
  writeLines(
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "#SBATCH --partition={{partition}}",
      "echo {{file_dir}} {{file_stem}} {{file_ext}} {{log_dir}}"
    ),
    path
  )
  model <- withr::local_tempfile(fileext = ".mod")
  file.create(model)
  dry <- submit_slurm_job(model, slurm_job_template_path = path, partition = "cpu2mem4gb", submission_root = "logs/here", dry_run = TRUE)
  expect_match(
    dry$template_script,
    sprintf("echo %s %s mod logs/here", fs::path_dir(model), fs::path_ext_remove(fs::path_file(model))),
    fixed = TRUE
  )
})

# --- the {{parallel}} builtin ---------------------------------------------------

test_that("parallel derives from whichever fill drives cpus-per-task", {
  tmpl <- Template() |> with_job_name() |> with_cpus_per_task("{{ncpu}}")
  expect_equal(derived_fills(list(ncpu = 1), template = tmpl)$parallel, FALSE)
  expect_equal(derived_fills(list(ncpu = 4), template = tmpl)$parallel, TRUE)
  expect_equal(derived_fills(list(ncpu = "4"), template = tmpl)$parallel, TRUE)
  # not yet filled, or no such flag at all: nothing to derive
  expect_null(derived_fills(list(), template = tmpl)$parallel)
  expect_null(derived_fills(list(ncpu = 4))$parallel)
  expect_null(derived_fills(list(ncpu = 4), template = Template() |> with_job_name())$parallel)
})

test_that("a differently-named cpus-per-task fill still drives parallel", {
  tmpl <- Template() |> with_job_name() |> with_sbatch_flag("cpus-per-task", "{{threads}}")
  expect_equal(derived_fills(list(threads = 8), template = tmpl)$parallel, TRUE)
})

test_that("{{#parallel}} resolves in the rendered script but stays literal at print time", {
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command("/opt/monolix/.../distMonolix", "{{file}}") |>
    with_post_run("echo", conditional = "parallel", if_true = "distributed", if_false = "single-node")

  expect_match(utils::tail(format(fill(tmpl, ncpu = 4)), 1), "\\{\\{/parallel\\}\\}", fixed = FALSE)

  one <- submit_slurm_job(tmpl, ncpu = 1, dry_run = TRUE, slurm_template_opts = list(file = "x.mlxtran", job_name = "m"))
  four <- submit_slurm_job(tmpl, ncpu = 4, dry_run = TRUE, slurm_template_opts = list(file = "x.mlxtran", job_name = "m"))
  expect_equal(utils::tail(strsplit(one$template_script, "\n")[[1]], 1), paste(echo, "single-node"))
  expect_equal(utils::tail(strsplit(four$template_script, "\n")[[1]], 1), paste(echo, "distributed"))
})

test_that("an explicit fill of parallel overrides the derived value", {
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command("echo", "hi") |>
    with_post_run("echo", conditional = "parallel", if_true = "yes")
  dry <- submit_slurm_job(tmpl, ncpu = 1, dry_run = TRUE, slurm_template_opts = list(job_name = "m", parallel = TRUE))
  expect_equal(utils::tail(strsplit(dry$template_script, "\n")[[1]], 1), paste(echo, "yes"))
})

test_that("{{parallel}} with no cpus-per-task flag is an ordinary unresolved placeholder", {
  tmpl <- Template() |> with_job_name() |> with_command("echo", "hi") |> with_post_run("echo", conditional = "parallel", if_true = "x")
  expect_error(submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "m")), "unfilled placeholders: `\\{\\{parallel\\}\\}`")
})

test_that("{{parallel}} used but cpus-per-task never filled hints at the real placeholder", {
  # named `threads`, not `ncpu`: an `ncpu` placeholder would take the default of 1
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("{{threads}}") |>
    with_command("echo", "hi") |>
    with_post_run("echo", conditional = "parallel", if_true = "x")
  expect_error(
    submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "m")),
    "slurm_template_opts = list(threads = ...)",
    fixed = TRUE
  )
  expect_error(submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "m")), "derives from `{{threads}}`", fixed = TRUE)
})
