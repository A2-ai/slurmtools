# slurm_template_opts()'s `required` column must mean "submit stops without
# it" (deep-review finding 5, decision 4). Each template is submitted dry with
# no fills at all; what submit's hint tells the user to pass
# (`slurm_template_opts = list(a = ..., b = ...)`) must be exactly what the
# table marks required. The message's first line names every unfilled tag as
# written, including ones that follow from the others; the hint is the list
# the user must give.

missing_at_submit <- function(tmpl) {
  local_partition_table()
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  err <- tryCatch(
    {
      submit_slurm_job(tmpl, dry_run = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (is.null(err)) {
    return(character())
  }
  expect_match(err, "unfilled placeholders", info = err)
  hint <- regmatches(err, regexpr("slurm_template_opts = list\\([^)]*\\)", err))
  expect_length(hint, 1)
  m <- regmatches(hint, gregexpr("([A-Za-z0-9_]+) = \\.\\.\\.", hint))[[1]]
  sub(" = \\.\\.\\.$", "", m)
}

expect_table_agrees_with_submit <- function(tmpl) {
  opts <- slurm_template_opts(tmpl)
  expect_setequal(opts$option[opts$required], missing_at_submit(tmpl))
}

test_that("the defaults submit fills in are not marked required", {
  tmpl <- default_template("echo", "{{file}}")
  opts <- slurm_template_opts(tmpl)
  row <- function(name) opts[opts$option == name, ]
  expect_false(row("ncpu")$required)
  expect_false(row("partition")$required)
  expect_false(row("job_name")$required)
  expect_true(row("file")$required)
  # and indeed only `file` is asked for
  expect_equal(missing_at_submit(tmpl), "file")
  expect_table_agrees_with_submit(tmpl)
})

test_that("parallel is required when there is no --cpus-per-task to derive it from", {
  no_cpu <- Template() |>
    with_job_name() |>
    with_partition() |>
    with_command("echo", "{{file}}", conditional("parallel", if_true = "--par"))
  opts <- slurm_template_opts(no_cpu)
  expect_true(opts$required[opts$option == "parallel"])
  expect_true(is.na(opts$default[opts$option == "parallel"]))
  expect_table_agrees_with_submit(no_cpu)

  with_cpu <- no_cpu |> with_cpus_per_task("{{ncpu}}")
  opts <- slurm_template_opts(with_cpu)
  expect_false(opts$required[opts$option == "parallel"])
  expect_equal(opts$default[opts$option == "parallel"], "TRUE when --cpus-per-task > 1")
  expect_table_agrees_with_submit(with_cpu)
})

test_that("a partition or cpu flag fed from another name is required: the dispatcher's defaults are by name", {
  tmpl <- Template() |>
    with_job_name("sim") |>
    with_partition("{{queue}}") |>
    with_cpus_per_task("{{threads}}") |>
    with_command("echo", "{{file}}")
  opts <- slurm_template_opts(tmpl)
  expect_true(opts$required[opts$option == "queue"])
  expect_true(opts$required[opts$option == "threads"])
  expect_table_agrees_with_submit(tmpl)
})

test_that("the table and submit agree on a template with every kind of placeholder", {
  tmpl <- default_template("echo", c("{{file}}", "--config", "{{config}}", "{{log_dir}}"),
                           conditional("parallel", if_true = "--par")) |>
    with_sbatch_flag("time", optional = TRUE) |>
    with_post_run("echo", "{{file_stem}}")
  expect_table_agrees_with_submit(tmpl)
  expect_table_agrees_with_submit(fill(tmpl, file = "sim.R"))
  expect_equal(missing_at_submit(fill(tmpl, file = "sim.R", config = "c.yaml")), character())
})

test_that("a job name with no `file` to take its stem from is required", {
  tmpl <- Template() |> with_job_name() |> with_partition() |> with_command("echo", "hi")
  opts <- slurm_template_opts(tmpl)
  expect_true(opts$required[opts$option == "job_name"])
  expect_table_agrees_with_submit(tmpl)
})

test_that("ncpu and partition are defaulted by name, flag or no flag, as the dispatcher does", {
  tmpl <- Template() |> with_job_name("x") |> with_command("echo", c("{{ncpu}}", "{{partition}}"))
  opts <- slurm_template_opts(tmpl)
  expect_false(opts$required[opts$option == "ncpu"])
  expect_false(opts$required[opts$option == "partition"])
  expect_table_agrees_with_submit(tmpl)
})

test_that("an optional flag's placeholder that the script also writes is required", {
  tmpl <- Template() |> with_job_name("x") |> with_account(optional = TRUE) |> with_command("echo", "{{account}}")
  opts <- slurm_template_opts(tmpl)
  expect_true(opts$required[opts$option == "account"])
  expect_table_agrees_with_submit(tmpl)
})

test_that("bbi_config_path is required unless the option is set", {
  tmpl <- Template() |> with_job_name("x") |> with_command("echo", "{{bbi_config_path}}")
  withr::local_options(slurmtools.bbi_config_path = NULL)
  expect_true(slurm_template_opts(tmpl)$required[slurm_template_opts(tmpl)$option == "bbi_config_path"])
  expect_table_agrees_with_submit(tmpl)
  withr::local_options(slurmtools.bbi_config_path = "/cfg/bbi.yaml")
  opts <- slurm_template_opts(tmpl)
  expect_false(opts$required[opts$option == "bbi_config_path"])
  expect_equal(opts$default[opts$option == "bbi_config_path"], "the slurmtools.bbi_config_path option")
  expect_table_agrees_with_submit(tmpl)
})

test_that("a switch the default ncpu settles is settled in the table too", {
  tmpl <- Template() |> with_job_name("x") |> with_partition() |> with_cpus_per_task("{{ncpu}}") |>
    with_command("echo", "run", conditional("parallel", if_true = "{{config}}", if_false = character()))
  opts <- slurm_template_opts(tmpl)
  # ncpu defaults to 1, so parallel is FALSE and {{config}} is in the branch not taken
  expect_false(opts$required[opts$option == "config"])
  expect_table_agrees_with_submit(tmpl)
})
