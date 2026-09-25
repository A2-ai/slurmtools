# --- with_pre_run() / with_post_run() ------------------------------------------

# no partition on purpose: nothing here needs the cluster
named <- function() Template() |> with_job_name()
rscript <- "/opt/R/4.5.3/bin/Rscript"
# how a hook's program is written: its path when the PATH has it, the bare name otherwise
hook_exe <- function(x) {
  found <- unname(Sys.which(x))
  if (nzchar(found)) found else x
}

test_that("hook lines land before and after the command whatever the call order", {
  tmpl <- named() |>
    with_post_run("echo", "done") |>
    with_command(rscript, "{{file}}") |>
    with_pre_run("module", c("load", "R/4.5")) |>
    with_pre_run("cd", "{{work_dir}}") |>
    with_pre_run("echo", "start") |>
    with_post_run("echo", c("really", "done"))

  expect_equal(
    format(tmpl),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "",
      "module load R/4.5", paste(hook_exe("cd"), "{{work_dir}}"), paste(hook_exe("echo"), "start"),
      "",
      paste(rscript, "{{file}}"),
      "",
      paste(hook_exe("echo"), "done"), paste(hook_exe("echo"), "really done")
    )
  )
  expect_length(tmpl@pre_run, 3)
  expect_length(tmpl@post_run, 2)
  expect_equal(tmpl@pre_run[[1]][c("exe", "args")], list(exe = "module", args = c("load", "R/4.5")))
})

test_that("a hook is a program plus arguments, in the same form as the command", {
  # a program on the PATH is written as its path; a shell builtin nobody can
  # find is written as given; a path and a placeholder are taken as they are
  tmpl <- named() |>
    with_pre_run("echo", "a") |>
    with_pre_run("module", c("load", "R")) |>
    with_pre_run("/opt/bin/tool", "--x") |>
    with_pre_run("{{hook}}", "{{file}}")
  expect_equal(
    format(tmpl)[4:7],
    c(paste(unname(Sys.which("echo")), "a"), "module load R", "/opt/bin/tool --x", "{{hook}} {{file}}")
  )
  # an argument with whitespace reaches the program as one word
  tmpl <- named() |> with_post_run("echo", "$SLURM_JOB_ID finished")
  expect_equal(utils::tail(format(tmpl), 1), paste(hook_exe("echo"), "\"$SLURM_JOB_ID finished\""))
})

test_that("a template without hooks renders exactly as before", {
  tmpl <- named() |> with_command(rscript, "{{file}}")
  expect_equal(format(tmpl), c("#!/bin/bash", "#SBATCH --job-name={{job_name}}", "", paste(rscript, "{{file}}")))
  expect_equal(tmpl@pre_run, list())
  expect_equal(tmpl@post_run, list())
})

test_that("hooks without a command still print, and pre lines come before post lines", {
  tmpl <- named() |> with_post_run("echo", "done") |> with_pre_run("echo", "start")
  expect_equal(
    format(tmpl),
    c("#!/bin/bash", "#SBATCH --job-name={{job_name}}", "", paste(hook_exe("echo"), "start"), "", paste(hook_exe("echo"), "done"))
  )
})

test_that("placeholders in hook lines are the template's placeholders", {
  tmpl <- named() |>
    with_pre_run("cd", "{{work_dir}}") |>
    with_post_run("curl", c("-s", "-d", "{{job_name}} finished", "ntfy.sh/{{ntfy_topic}}"))

  expect_setequal(template_placeholders(tmpl), c("job_name", "work_dir", "ntfy_topic"))

  filled <- fill(tmpl, work_dir = "/data/sim", ntfy_topic = "tariq-jobs", job_name = "sim")
  expect_true(paste(hook_exe("cd"), "/data/sim") %in% format(filled))
  expect_true(paste(hook_exe("curl"), "-s -d \"sim finished\" ntfy.sh/tariq-jobs") %in% format(filled))

  expect_error(fill(tmpl, wrk_dir = "x"), "no placeholder `{{wrk_dir}}`", fixed = TRUE)
  expect_error(fill(tmpl, wrk_dir = "x"), "`work_dir`", fixed = TRUE)
})

test_that("a hook placeholder left unfilled blocks submission", {
  tmpl <- named() |>
    with_command(rscript, "{{file}}") |>
    with_post_run("cp", c("{{file}}", "{{archive}}/"))
  expect_error(
    submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", job_name = "sim")),
    "unfilled placeholders: `{{archive}}`",
    fixed = TRUE
  )
})

test_that("the rendered job script runs pre lines, the command, then post lines", {
  tmpl <- named() |>
    with_pre_run("echo", "start") |>
    with_command(rscript, "{{file}}") |>
    with_post_run("echo", "done")
  dry <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", job_name = "sim"))
  script <- strsplit(dry$template_script, "\n")[[1]]
  expect_equal(
    utils::tail(script, 5),
    c(paste(hook_exe("echo"), "start"), "", paste(rscript, "sim.R"), "", paste(hook_exe("echo"), "done"))
  )
})

test_that("a hook takes a conditional like the command does", {
  tmpl <- named() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command(rscript, "{{file}}") |>
    with_post_run("echo", conditional = "parallel", if_true = "distributed", if_false = "single-node")
  one <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "s", job_name = "s", ncpu = 1))
  four <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "s", job_name = "s", ncpu = 4))
  expect_equal(utils::tail(strsplit(one$template_script, "\n")[[1]], 1), paste(hook_exe("echo"), "single-node"))
  expect_equal(utils::tail(strsplit(four$template_script, "\n")[[1]], 1), paste(hook_exe("echo"), "distributed"))
})

test_that("hooks alone are not a job", {
  tmpl <- named() |> with_pre_run("echo", "start") |> with_post_run("echo", "done")
  expect_error(submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(job_name = "x")), "no command to run")
})

test_that("write_slurm_template() writes the hooks into the file", {
  path <- withr::local_tempfile(fileext = ".tmpl")
  named() |>
    with_pre_run("module", c("load", "R/4.5")) |>
    with_command(rscript, "{{file}}") |>
    with_post_run("echo", "done") |>
    write_slurm_template(path) |>
    suppressMessages()
  lines <- readLines(path)
  done <- paste(hook_exe("echo"), "done")
  expect_true("module load R/4.5" %in% lines)
  expect_true(done %in% lines)
  expect_lt(which(lines == "module load R/4.5"), which(lines == paste(rscript, "{{file}}")))
  expect_gt(which(lines == done), which(lines == paste(rscript, "{{file}}")))
})

test_that("with_pre_run() / with_post_run() check their input", {
  expect_error(with_pre_run(named(), character()), "`command` must be a single program")
  expect_error(with_pre_run(named(), NA_character_), "`command` must be a single program")
  expect_error(with_post_run(named(), 1), "`command` must be a single program")
  expect_error(with_post_run(named(), "echo", list("x")), "`args` must be a character vector")
  expect_error(with_post_run(named(), "echo", if_true = "x"), "need a `conditional`")
  expect_error(with_post_run("not a template", "x"), "must be a Template()", fixed = TRUE)
})
