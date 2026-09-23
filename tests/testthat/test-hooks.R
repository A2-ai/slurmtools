# --- with_pre_run() / with_post_run() ------------------------------------------

# no partition on purpose: nothing here needs the cluster
named <- function() Template() |> with_job_name()
rscript <- "/opt/R/4.5.3/bin/Rscript"

test_that("hook lines land before and after the command whatever the call order", {
  tmpl <- named() |>
    with_post_run("echo done") |>
    with_command(rscript, "{{file}}") |>
    with_pre_run(c("module load R/4.5", "cd {{work_dir}}")) |>
    with_pre_run("echo start") |>
    with_post_run("echo really done")

  expect_equal(
    format(tmpl),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "",
      "module load R/4.5", "cd {{work_dir}}", "echo start",
      "",
      paste(rscript, "{{file}}"),
      "",
      "echo done", "echo really done"
    )
  )
  expect_equal(tmpl@pre_run, c("module load R/4.5", "cd {{work_dir}}", "echo start"))
  expect_equal(tmpl@post_run, c("echo done", "echo really done"))
})

test_that("a template without hooks renders exactly as before", {
  tmpl <- named() |> with_command(rscript, "{{file}}")
  expect_equal(format(tmpl), c("#!/bin/bash", "#SBATCH --job-name={{job_name}}", "", paste(rscript, "{{file}}")))
  expect_equal(tmpl@pre_run, character())
  expect_equal(tmpl@post_run, character())
})

test_that("hooks without a command still print, and pre lines come before post lines", {
  tmpl <- named() |> with_post_run("echo done") |> with_pre_run("echo start")
  expect_equal(format(tmpl), c("#!/bin/bash", "#SBATCH --job-name={{job_name}}", "", "echo start", "", "echo done"))
})

test_that("placeholders in hook lines are the template's placeholders", {
  tmpl <- named() |>
    with_pre_run("cd {{work_dir}}") |>
    with_post_run("curl -s -d '{{job_name}} finished' ntfy.sh/{{ntfy_topic}}")

  expect_setequal(template_placeholders(tmpl), c("job_name", "work_dir", "ntfy_topic"))

  filled <- fill(tmpl, work_dir = "/data/sim", ntfy_topic = "tariq-jobs", job_name = "sim")
  expect_true("cd /data/sim" %in% format(filled))
  expect_true("curl -s -d 'sim finished' ntfy.sh/tariq-jobs" %in% format(filled))

  expect_error(fill(tmpl, wrk_dir = "x"), "no placeholder `{{wrk_dir}}`", fixed = TRUE)
  expect_error(fill(tmpl, wrk_dir = "x"), "`work_dir`", fixed = TRUE)
})

test_that("a hook placeholder left unfilled blocks submission", {
  tmpl <- named() |>
    with_command(rscript, "{{file}}") |>
    with_post_run("cp {{file}} {{archive}}/")
  expect_error(
    submit_slurm_job(tmpl, file = "sim.R", job_name = "sim", dry_run = TRUE),
    "unfilled placeholders: `{{archive}}`",
    fixed = TRUE
  )
})

test_that("the rendered job script runs pre lines, the command, then post lines", {
  tmpl <- named() |>
    with_pre_run("echo start") |>
    with_command(rscript, "{{file}}") |>
    with_post_run("echo done")
  dry <- submit_slurm_job(tmpl, file = "sim.R", job_name = "sim", dry_run = TRUE)
  script <- strsplit(dry$template_script, "\n")[[1]]
  expect_equal(
    utils::tail(script, 5),
    c("echo start", "", paste(rscript, "sim.R"), "", "echo done")
  )
})

test_that("hooks alone are not a job", {
  tmpl <- named() |> with_pre_run("echo start") |> with_post_run("echo done")
  expect_error(submit_slurm_job(tmpl, job_name = "x", dry_run = TRUE), "no command to run")
})

test_that("write_slurm_template() writes the hooks into the file", {
  path <- withr::local_tempfile(fileext = ".tmpl")
  named() |>
    with_pre_run("module load R/4.5") |>
    with_command(rscript, "{{file}}") |>
    with_post_run("echo done") |>
    write_slurm_template(path) |>
    suppressMessages()
  lines <- readLines(path)
  expect_true("module load R/4.5" %in% lines)
  expect_true("echo done" %in% lines)
  expect_lt(which(lines == "module load R/4.5"), which(lines == paste(rscript, "{{file}}")))
  expect_gt(which(lines == "echo done"), which(lines == paste(rscript, "{{file}}")))
})

test_that("with_pre_run() / with_post_run() check their input", {
  expect_error(with_pre_run(named(), character()), "one or more bash lines")
  expect_error(with_pre_run(named(), NA_character_), "one or more bash lines")
  expect_error(with_post_run(named(), 1), "one or more bash lines")
  expect_error(with_post_run(named(), list("x")), "one or more bash lines")
  expect_error(with_post_run("not a template", "x"), "must be a Template()", fixed = TRUE)
})
