# --- with_command(launcher =, launcher_args =) ---------------------------------

rscript <- "/opt/R/4.5.3/bin/Rscript"

test_that("a launcher is prepended, resolved the same way command is", {
  tmpl <- Template() |> with_job_name() |> with_command(rscript, "{{file}}", launcher = "env")
  expect_equal(utils::tail(format(tmpl), 1), paste(Sys.which("env"), rscript, "{{file}}"))
})

test_that("launcher_args sit between the launcher and the command", {
  tmpl <- Template() |>
    with_job_name() |>
    with_command(
      "/opt/monolix/MonolixSuite2024R1/lib/distMonolix", c("-p", "{{file}}", "--thread", "{{ncpu}}"),
      launcher = "env", launcher_args = c("FOO=bar", "--bind-to", "core")
    )
  expect_equal(
    utils::tail(format(tmpl), 1),
    paste(
      Sys.which("env"), "FOO=bar --bind-to core",
      "/opt/monolix/MonolixSuite2024R1/lib/distMonolix -p {{file}} --thread {{ncpu}}"
    )
  )
})

test_that("a launcher containing a slash skips PATH resolution", {
  tmpl <- Template() |> with_job_name() |> with_command(rscript, "{{file}}", launcher = "/opt/openmpi/bin/mpirun")
  expect_match(utils::tail(format(tmpl), 1), "^/opt/openmpi/bin/mpirun ", fixed = FALSE)
})

test_that("a launcher not on the PATH fails fast, naming it", {
  expect_error(
    Template() |> with_job_name() |> with_command(rscript, "{{file}}", launcher = "no-such-launcher-xyz"),
    "could not find `no-such-launcher-xyz`"
  )
})

test_that("a second with_command() call is unaffected by the first one's launcher", {
  tmpl <- Template() |>
    with_job_name() |>
    with_command("echo", "starting", launcher = "env") |>
    with_command(rscript, "{{file}}")
  cmd_lines <- utils::tail(format(tmpl), 2)
  expect_equal(cmd_lines[1], paste(Sys.which("env"), Sys.which("echo"), "starting"))
  expect_equal(cmd_lines[2], paste(rscript, "{{file}}"))
})

test_that("with_command() checks the launcher arguments", {
  # each launcher argument is one element, never several in one string
  expect_error(with_command(Template(), rscript, launcher = 1), "`launcher` must be")
  expect_error(with_command(Template(), rscript, launcher = NA_character_), "`launcher` must be")
  expect_error(with_command(Template(), rscript, launcher = character()), "`launcher` must be")
  expect_error(with_command(Template(), rscript, launcher_args = 1), "`launcher_args` must be")
  expect_error(with_command(Template(), rscript, launcher_args = "x"), "needs a `launcher`", fixed = TRUE)
})

test_that("the corpus ex. 2 shape submits with the launcher line resolved and fanned out", {
  tmpl <- Template() |>
    with_job_name() |>
    with_nodes() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command(
      "/opt/monolix/MonolixSuite2024R1/lib/distMonolix", c("-p", "{{file}}", "--thread", "{{ncpu}}"),
      launcher = "env", launcher_args = c("--map-by", "slot:PE={{ncpu}}")
    )
  dry <- submit_slurm_job(
    tmpl, ncpu = 2, dry_run = TRUE,
    slurm_template_opts = list(file = "theophylline_project.mlxtran", job_name = "monolixRun", nodes = 2)
  )
  script <- strsplit(dry$template_script, "\n")[[1]]
  expect_equal(
    utils::tail(script, 1),
    sprintf(
      "%s --map-by slot:PE=2 /opt/monolix/MonolixSuite2024R1/lib/distMonolix -p theophylline_project.mlxtran --thread 2",
      Sys.which("env")
    )
  )
})
