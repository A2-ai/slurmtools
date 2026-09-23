# --- Template() + with_sbatch() ---------------------------------------------

test_that("an empty Template renders to a bare bash shebang", {
  expect_equal(format(Template()), "#!/bin/bash")
})

test_that("with_sbatch adds one #SBATCH line per flag, in call order", {
  tmpl <- Template() |>
    with_sbatch("job-name") |>
    with_sbatch("cpus-per-task", "ncpu") |>
    with_sbatch("partition")

  expect_equal(
    format(tmpl),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}"
    )
  )
})

test_that("the default placeholder is the flag with dashes as underscores", {
  tmpl <- Template() |> with_sbatch("ntasks-per-node")
  expect_equal(tmpl@sbatch, c("ntasks-per-node" = "ntasks_per_node"))
})

test_that("adding a flag twice replaces its placeholder in place", {
  tmpl <- Template() |>
    with_sbatch("nodes") |>
    with_sbatch("partition") |>
    with_sbatch("nodes", "n_nodes")
  expect_equal(tmpl@sbatch, c(nodes = "n_nodes", partition = "partition"))
})

test_that("flags are sbatch long options without dashes", {
  expect_error(Template() |> with_sbatch("--nodes"), "leading dashes")
  expect_error(Template() |> with_sbatch("Nodes"), "leading dashes")
  expect_error(
    Template() |> with_sbatch(c("nodes", "ntasks")),
    "single sbatch option"
  )
})

test_that("placeholder names must work as R argument names", {
  expect_error(
    Template() |> with_sbatch("cpus-per-task", "2cpu"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch("gres", "gpu-count"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch("gres", c("a", "b")),
    "single placeholder name"
  )
})

test_that("with_sbatch needs a Template", {
  expect_error(with_sbatch("rscript.tmpl", "partition"), "must be a Template")
})

test_that("Template() validates flags passed directly", {
  expect_error(Template(sbatch = c("a", "b")), "named character vector")
  expect_error(Template(sbatch = c(nodes = "n", nodes = "m")), "once")
})

test_that("print shows the rendered header", {
  expect_output(
    print(Template() |> with_sbatch("partition")),
    "#SBATCH --partition={{partition}}",
    fixed = TRUE
  )
})

# --- fill() -------------------------------------------------------------------

test_that("fill renders a value in place and leaves the rest as placeholders", {
  tmpl <- Template() |>
    with_sbatch("job-name") |>
    with_sbatch("cpus-per-task", "ncpu") |>
    with_sbatch("partition") |>
    fill(job_name = "10001")

  expect_equal(
    format(tmpl)[-1],
    c(
      "#SBATCH --job-name=10001",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}"
    )
  )
})

test_that("one placeholder can feed several flags", {
  tmpl <- Template() |>
    with_sbatch("cpus-per-task", "ncpu") |>
    with_sbatch("ntasks", "ncpu") |>
    fill(ncpu = 4)
  expect_equal(
    format(tmpl)[-1],
    c("#SBATCH --cpus-per-task=4", "#SBATCH --ntasks=4")
  )
})

test_that("numbers render as sbatch expects them, never in scientific notation", {
  tmpl <- Template() |> with_sbatch("mem") |> fill(mem = 100000)
  expect_equal(format(tmpl)[2], "#SBATCH --mem=100000")
})

test_that("filling a placeholder again replaces the earlier value", {
  tmpl <- Template() |>
    with_sbatch("partition") |>
    fill(partition = "a") |>
    fill(partition = "b")
  expect_equal(tmpl@fills, list(partition = "b"))
  expect_equal(format(tmpl)[2], "#SBATCH --partition=b")
})

test_that("fill refuses placeholders the template does not have, and lists the real ones", {
  tmpl <- Template() |>
    with_sbatch("partition") |>
    with_sbatch("cpus-per-task", "ncpu")
  expect_error(fill(tmpl, partiton = "x"), "no placeholder `{{partiton}}`", fixed = TRUE)
  expect_error(fill(tmpl, partiton = "x"), "`partition`, `ncpu`", fixed = TRUE)
  expect_error(fill(Template(), ncpu = 1), "no placeholders yet")
})

test_that("fill needs named, single values", {
  tmpl <- Template() |> with_sbatch("partition")
  expect_error(fill(tmpl, "cpu2mem4gb"), "must be named")
  expect_error(fill(tmpl, partition = c("a", "b")), "single non-missing value")
  expect_error(fill(tmpl, partition = NULL), "single non-missing value")
  expect_error(fill(tmpl, partition = NA), "single non-missing value")
  expect_error(fill("rscript.tmpl", partition = "x"), "must be a Template")
})

test_that("fill with nothing to fill returns the template unchanged", {
  tmpl <- Template() |> with_sbatch("partition")
  expect_identical(fill(tmpl), tmpl)
})

test_that("a fill for a placeholder the template lacks is caught, however it gets there", {
  expect_error(
    Template(sbatch = c(nodes = "nodes"), fills = list(ncpu = 2)),
    "does not have"
  )
  tmpl <- Template() |> with_sbatch("nodes") |> fill(nodes = 2)
  expect_error(with_sbatch(tmpl, "nodes", "n_nodes"), "does not have")
})

# --- write_slurm_template() ---------------------------------------------------

test_that("write_slurm_template writes the rendered lines, newline-terminated", {
  path <- withr::local_tempfile(fileext = ".tmpl")
  tmpl <- Template() |>
    with_sbatch("job-name") |>
    with_sbatch("partition") |>
    fill(partition = "cpu2mem4gb")

  out <- suppressMessages(write_slurm_template(tmpl, path))

  expect_identical(out, path)
  expect_equal(
    brio::read_file(path),
    "#!/bin/bash\n#SBATCH --job-name={{job_name}}\n#SBATCH --partition=cpu2mem4gb\n"
  )
})

test_that("write_slurm_template refuses to overwrite unless asked", {
  path <- withr::local_tempfile(fileext = ".tmpl", lines = "keep me")
  tmpl <- Template() |> with_sbatch("partition")

  expect_error(write_slurm_template(tmpl, path), "already exists")
  expect_equal(readLines(path), "keep me")

  suppressMessages(write_slurm_template(tmpl, path, overwrite = TRUE))
  expect_equal(readLines(path)[1], "#!/bin/bash")
})

test_that("write_slurm_template needs a Template", {
  expect_error(write_slurm_template("rscript.tmpl", "out.tmpl"), "must be a Template")
})

test_that("a written template submits through submit_slurm_job() as it stands today", {
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  path <- withr::local_tempfile(fileext = ".tmpl")
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  Template() |>
    with_sbatch("job-name") |>
    with_sbatch("partition") |>
    with_sbatch("cpus-per-task", "ncpu") |>
    write_slurm_template(path) |>
    suppressMessages()
  # the command line is the user's to add; the file is theirs
  cat("echo {{file}}\n", file = path, append = TRUE)

  cmd <- submit_slurm_job(
    script,
    template = path,
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE
  )

  expect_match(cmd$template_script, "#SBATCH --partition=cpu2mem4gb", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --cpus-per-task=2", fixed = TRUE)
  expect_match(cmd$template_script, paste("echo", script), fixed = TRUE)
})

# --- named with_*() verbs -------------------------------------------------------

test_that("the named verbs and the flag table stay in step, both directions", {
  verbs <- setdiff(
    ls(asNamespace("slurmtools"), pattern = "^with_"),
    c("with_sbatch", "with_command", "with_pre_run", "with_post_run") # the verbs that are not a flag
  )
  expect_setequal(verbs, paste0("with_", gsub("-", "_", names(sbatch_flags))))
  for (flag in names(sbatch_flags)) {
    verb <- get(paste0("with_", gsub("-", "_", flag)))
    expect_equal(
      verb(Template())@sbatch,
      stats::setNames(gsub("-", "_", flag), flag),
      info = flag
    )
    expect_equal(verb(Template(), "x")@sbatch, stats::setNames("x", flag), info = flag)
  }
})

test_that("the meeting's sketch renders through the named verbs", {
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("ncpu") |>
    with_partition()
  expect_equal(
    format(tmpl)[-1],
    c(
      "#SBATCH --job-name={{job_name}}",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}"
    )
  )
  expect_equal(
    format(fill(tmpl, job_name = "10001", ncpu = 4, partition = "cpu8mem32gb"))[-1],
    c(
      "#SBATCH --job-name=10001",
      "#SBATCH --cpus-per-task=4",
      "#SBATCH --partition=cpu8mem32gb"
    )
  )
})

test_that("count flags need a positive whole number", {
  tmpl <- Template() |> with_cpus_per_task("ncpu")
  expect_error(fill(tmpl, ncpu = 0), "`--cpus-per-task`, which needs a positive whole number; got 0", fixed = TRUE)
  expect_error(fill(tmpl, ncpu = 2.5), "positive whole number")
  expect_error(fill(tmpl, ncpu = "4"), "positive whole number")
  expect_equal(format(fill(tmpl, ncpu = 4L))[2], "#SBATCH --cpus-per-task=4")
  expect_error(fill(with_nodes(Template()), nodes = -1), "`--nodes`", fixed = TRUE)
})

test_that("name-like flags need a non-empty string; job-name takes any value", {
  expect_error(fill(with_partition(Template()), partition = 3), "`--partition`, which needs a non-empty string", fixed = TRUE)
  expect_error(fill(with_gres(Template()), gres = " "), "non-empty string")
  expect_equal(format(fill(with_job_name(Template()), job_name = 1001))[2], "#SBATCH --job-name=1001")
})

test_that("a placeholder feeding two flags must satisfy both", {
  tmpl <- Template() |> with_cpus_per_task("n") |> with_partition("n")
  expect_error(fill(tmpl, n = 4), "`--partition`", fixed = TRUE)
  expect_error(fill(tmpl, n = "cpu2mem4gb"), "`--cpus-per-task`", fixed = TRUE)
})

test_that("the check follows the flag, not the verb that added it", {
  expect_error(fill(with_sbatch(Template(), "nodes"), nodes = 0), "positive whole number")
  expect_error(
    Template(sbatch = c(nodes = "nodes"), fills = list(nodes = 0)),
    "positive whole number"
  )
  # a flag outside the table is not checked
  expect_equal(
    format(fill(with_sbatch(Template(), "time"), time = "not-a-time"))[2],
    "#SBATCH --time=not-a-time"
  )
})

# --- optional flags -------------------------------------------------------------

test_that("an unfilled optional flag renders inside a section, in its place", {
  tmpl <- Template() |>
    with_partition() |>
    with_account(optional = TRUE) |>
    with_cpus_per_task("ncpu")
  expect_equal(
    format(tmpl)[-1],
    c(
      "#SBATCH --partition={{partition}}",
      "{{#account}}",
      "#SBATCH --account={{account}}",
      "{{/account}}",
      "#SBATCH --cpus-per-task={{ncpu}}"
    )
  )
  expect_equal(tmpl@optional, "account")
})

test_that("a filled optional flag renders as a plain line", {
  tmpl <- Template() |> with_account(optional = TRUE) |> fill(account = "proj-x")
  expect_equal(format(tmpl)[-1], "#SBATCH --account=proj-x")
})

test_that("required flags never get a section, and optional works on the blanket verb too", {
  expect_equal(format(with_account(Template()))[-1], "#SBATCH --account={{account}}")
  expect_equal(
    format(with_sbatch(Template(), "time", optional = TRUE))[-1],
    c("{{#time}}", "#SBATCH --time={{time}}", "{{/time}}")
  )
})

test_that("re-adding a flag resets its optional setting", {
  tmpl <- Template() |> with_account(optional = TRUE) |> with_account()
  expect_equal(tmpl@optional, character())
  tmpl <- Template() |> with_nodes() |> with_nodes(optional = TRUE)
  expect_equal(tmpl@optional, "nodes")
})

test_that("optional must be TRUE or FALSE, and must name a flag the template has", {
  expect_error(with_account(Template(), optional = NA), "TRUE or FALSE")
  expect_error(with_account(Template(), optional = "yes"), "TRUE or FALSE")
  expect_error(
    Template(sbatch = c(nodes = "nodes"), optional = "account"),
    "optional names flags the template does not have: `account`",
    fixed = TRUE
  )
  expect_error(
    Template(sbatch = c(nodes = "nodes"), optional = c("nodes", "nodes")),
    "once"
  )
})

test_that("a written optional flag is dropped or kept by submit_slurm_job() as today", {
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  path <- withr::local_tempfile(fileext = ".tmpl")
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  Template() |>
    with_job_name() |>
    with_partition() |>
    with_account(optional = TRUE) |>
    with_cpus_per_task("ncpu") |>
    write_slurm_template(path) |>
    suppressMessages()
  cat("echo {{file}}\n", file = path, append = TRUE)

  without <- submit_slurm_job(script, template = path, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_no_match(without$template_script, "--account", fixed = TRUE)
  expect_match(without$template_script, "#SBATCH --partition=cpu2mem4gb\n#SBATCH --cpus-per-task=1", fixed = TRUE)

  with <- submit_slurm_job(
    script,
    template = path,
    partition = "cpu2mem4gb",
    account = "proj-x",
    dry_run = TRUE
  )
  expect_match(with$template_script, "#SBATCH --account=proj-x", fixed = TRUE)
})

# --- with_command() -----------------------------------------------------------

echo_path <- unname(Sys.which("echo"))

test_that("with_command resolves the program and keeps its args, placeholders included", {
  tmpl <- Template() |> with_command("echo", c("run", "{{file}}"))
  expect_equal(tmpl@body, paste(echo_path, "run {{file}}"))

  tmpl <- with_command(tmpl, "/opt/tool/bin/tool", "--flag")
  expect_equal(tmpl@body[2], "/opt/tool/bin/tool --flag")

  expect_error(with_command(Template(), "not-a-real-tool"), "could not find")
  expect_error(with_command(Template(), c("a", "b")), "single program")
  expect_error(with_command(Template(), "echo", 1), "character vector")
  expect_error(with_command("x", "echo"), "must be a Template")
})

test_that("body placeholders are fillable and render after a blank line", {
  tmpl <- Template() |> with_partition() |> with_command("echo", "{{file}}")
  expect_error(fill(tmpl, nope = 1), "`partition`, `file`", fixed = TRUE)
  expect_equal(
    format(fill(tmpl, file = "sim.R"))[-1],
    c("#SBATCH --partition={{partition}}", "", paste(echo_path, "sim.R"))
  )
})

test_that("a placeholder shared by header and body is filled in both", {
  tmpl <- Template() |>
    with_job_name("run") |>
    with_command("echo", "{{run}}") |>
    fill(run = "1001")
  expect_equal(
    format(tmpl)[c(2, 4)],
    c("#SBATCH --job-name=1001", paste(echo_path, "1001"))
  )
})

# --- submit_slurm_job(<Template>) ---------------------------------------------

sketch <- function() {
  Template() |>
    with_job_name() |>
    with_partition() |>
    with_cpus_per_task("ncpu") |>
    with_account(optional = TRUE) |>
    with_command("echo", "{{file}}")
}

test_that("a Template submits with `...` as the final fill (dry run)", {
  root <- withr::local_tempdir()
  withr::local_options(slurmtools.submission_root = root)
  cmd <- submit_slurm_job(
    sketch(),
    file = "sim.R",
    job_name = "sim",
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE
  )
  expect_equal(
    strsplit(cmd$template_script, "\n")[[1]],
    c(
      "#!/bin/bash",
      "#SBATCH --job-name=sim",
      "#SBATCH --partition=cpu2mem4gb",
      "#SBATCH --cpus-per-task=2",
      sprintf("#SBATCH --output=%s/sim-%%j.out", root),
      sprintf("#SBATCH --error=%s/sim-%%j.err", root),
      "",
      paste(echo_path, "sim.R")
    )
  )
  expect_equal(cmd$partition, "cpu2mem4gb")
  expect_equal(cmd$args, c("--parsable", file.path(root, "sim.sh")))
})

test_that("a template's own --output / --error are kept, each defaulted on its own", {
  root <- withr::local_tempdir()
  withr::local_options(slurmtools.submission_root = root)
  tmpl <- sketch() |> with_output("log_path")
  cmd <- submit_slurm_job(
    tmpl,
    file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 1, log_path = "mine-%j.log",
    dry_run = TRUE
  )
  expect_match(cmd$template_script, "#SBATCH --output=mine-%j.log", fixed = TRUE)
  expect_no_match(cmd$template_script, "s-%j.out", fixed = TRUE)
  expect_match(cmd$template_script, sprintf("#SBATCH --error=%s/s-%%j.err", root), fixed = TRUE)
})

test_that("pre-filled and submit-time fills combine; submit-time wins", {
  tmpl <- sketch() |> fill(job_name = "a", ncpu = 1)
  cmd <- submit_slurm_job(tmpl, file = "sim.R", partition = "cpu2mem4gb", ncpu = 2, dry_run = TRUE)
  expect_match(cmd$template_script, "--cpus-per-task=2", fixed = TRUE)
  expect_match(cmd$template_script, "--job-name=a", fixed = TRUE)
})

test_that("unfilled required placeholders stop the submit; unfilled optional ones are dropped", {
  expect_error(
    submit_slurm_job(sketch(), job_name = "sim", partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE),
    "unfilled placeholders: `{{file}}`",
    fixed = TRUE
  )
  expect_error(
    # partition and ncpu fall back to their defaults; the other two do not
    submit_slurm_job(sketch(), dry_run = TRUE),
    "`{{job_name}}`, `{{file}}`",
    fixed = TRUE
  )
  cmd <- submit_slurm_job(sketch(), file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE)
  expect_no_match(cmd$template_script, "account", fixed = TRUE)
  cmd <- submit_slurm_job(
    sketch(),
    file = "s",
    job_name = "s",
    partition = "cpu2mem4gb",
    ncpu = 1,
    account = "proj-x",
    dry_run = TRUE
  )
  expect_match(cmd$template_script, "#SBATCH --account=proj-x", fixed = TRUE)
})

test_that("a template with no command is refused", {
  tmpl <- Template() |> with_partition()
  expect_error(submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE), "no command")
})

test_that("file-form arguments are refused with a Template", {
  expect_error(submit_slurm_job(sketch(), template = "x.tmpl", dry_run = TRUE), "file form")
  expect_error(submit_slurm_job(sketch(), template_opts = list(a = 1), dry_run = TRUE), "file form")
})

test_that("ncpu goes to a {{ncpu}} placeholder, or fails naming the real ones", {
  tmpl <- Template() |> with_partition() |> with_cpus_per_task() |> with_command("echo", "hi")
  expect_error(
    submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 2, dry_run = TRUE),
    "no placeholder `{{ncpu}}`",
    fixed = TRUE
  )
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", cpus_per_task = 2, dry_run = TRUE)
  expect_match(cmd$template_script, "--cpus-per-task=2", fixed = TRUE)
})

test_that("the partition is validated and cpus checked against it, as for files", {
  expect_error(
    submit_slurm_job(sketch(), file = "s", job_name = "s", partition = "not-a-partition", ncpu = 1, dry_run = TRUE),
    "not an available partition"
  )
  expect_error(
    submit_slurm_job(sketch(), file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 64, dry_run = TRUE),
    "greater than number of available CPUs"
  )
})

# a stub sbatch on the PATH that answers `--parsable` style, so nothing is submitted
stub_sbatch <- function(says, env = parent.frame()) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  brio::write_file(sprintf("#!/bin/bash\necho '%s'\n", says), file.path(stub_dir, "sbatch"))
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
}

test_that("a real submit writes <job_name>.sh, calls sbatch --parsable and returns a Job", {
  root <- withr::local_tempdir()
  stub_sbatch("12345")

  job <- submit_slurm_job(
    sketch(),
    file = "sim.R",
    job_name = "sim",
    partition = "cpu2mem4gb",
    ncpu = 1,
    submission_root = root
  )

  expect_true(S7::S7_inherits(job, Job))
  expect_equal(job@job_id, "12345")
  expect_equal(job@job_name, "sim")
  expect_equal(job@partition, "cpu2mem4gb")
  expect_equal(job@script, file.path(root, "sim.sh"))
  expect_equal(job@output, file.path(root, "sim-12345.out"))
  expect_equal(job@error, file.path(root, "sim-12345.err"))
  expect_s3_class(job@submit_time, "POSIXct")
  expect_equal(job@sbatch$status, 0L)

  script <- brio::read_file(job@script)
  expect_match(script, "#SBATCH --job-name=sim", fixed = TRUE)
  expect_match(script, sprintf("#SBATCH --output=%s/sim-%%j.out", root), fixed = TRUE)
  expect_match(script, sprintf("#SBATCH --error=%s/sim-%%j.err", root), fixed = TRUE)

  expect_output(print(job), "<Job> 12345 \u00b7 sim \u00b7 cpu2mem4gb \u00b7 submitted 20", fixed = TRUE)
  expect_output(print(job), file.path(root, "sim-12345.out"), fixed = TRUE)
})

test_that("the job id is read off sbatch --parsable output, cluster suffix and all", {
  root <- withr::local_tempdir()
  stub_sbatch("77;cluster1")
  job <- submit_slurm_job(sketch(), file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 1, submission_root = root)
  expect_equal(job@job_id, "77")
  expect_equal(job@output, file.path(root, "s-77.out"))
})

test_that("an sbatch that returns no id is reported, not swallowed", {
  root <- withr::local_tempdir()
  stub_sbatch("Submitted batch job 9")
  expect_error(
    submit_slurm_job(sketch(), file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 1, submission_root = root),
    "did not return a job id"
  )
})

test_that("a template's own log path lands in the handle with %j resolved", {
  root <- withr::local_tempdir()
  stub_sbatch("5")
  tmpl <- sketch() |> with_output("log_path")
  job <- submit_slurm_job(
    tmpl,
    file = "s", job_name = "s", partition = "cpu2mem4gb", ncpu = 1, log_path = "mine-%j.log",
    submission_root = root
  )
  expect_equal(job@output, "mine-5.log")
  expect_equal(job@error, file.path(root, "s-5.err"))
})

test_that("without a job-name flag the script is slurm-job.sh", {
  tmpl <- Template() |> with_partition() |> with_command("echo", "hi")
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_equal(basename(cmd$args[[2]]), "slurm-job.sh")
  expect_match(cmd$template_script, "slurm-job-%j.out", fixed = TRUE)
})
