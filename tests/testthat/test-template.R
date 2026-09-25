# --- Template() + with_sbatch_flag() -------------------------------------------

test_that("an empty Template renders to a bare bash shebang", {
  expect_equal(format(Template()), "#!/bin/bash")
})

test_that("with_sbatch_flag adds one #SBATCH line per flag, in call order", {
  tmpl <- Template() |>
    with_sbatch_flag("job-name") |>
    with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
    with_sbatch_flag("partition")

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
  tmpl <- Template() |> with_sbatch_flag("ntasks-per-node")
  expect_equal(tmpl@sbatch, c("ntasks-per-node" = "ntasks_per_node"))
})

test_that("adding a flag twice replaces its placeholder in place", {
  tmpl <- Template() |>
    with_sbatch_flag("nodes") |>
    with_sbatch_flag("partition") |>
    with_sbatch_flag("nodes", "{{n_nodes}}")
  expect_equal(tmpl@sbatch, c(nodes = "n_nodes", partition = "partition"))
})

test_that("flags are sbatch long options without dashes", {
  expect_error(Template() |> with_sbatch_flag("--nodes"), "leading dashes")
  expect_error(Template() |> with_sbatch_flag("Nodes"), "leading dashes")
  expect_error(
    Template() |> with_sbatch_flag(c("nodes", "ntasks")),
    "single sbatch option"
  )
})

test_that("placeholder names must work as R argument names", {
  expect_error(
    Template() |> with_sbatch_flag("cpus-per-task", "{{2cpu}}"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch_flag("gres", "{{gpu-count}}"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch_flag("gres", c("a", "b")),
    "single value"
  )
})

test_that("a verb's argument is the flag's value", {
  tmpl <- Template() |>
    with_job_name("nm1") |>
    with_cpus_per_task(4) |>
    with_account("proj-x", optional = TRUE) |>
    with_sbatch_flag("time", "01:00:00")
  expect_equal(
    format(tmpl),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name=nm1",
      "#SBATCH --cpus-per-task=4",
      "#SBATCH --account=proj-x",
      "#SBATCH --time=01:00:00"
    )
  )
  expect_equal(tmpl@fills, list(job_name = "nm1", cpus_per_task = 4, account = "proj-x", time = "01:00:00"))
  expect_equal(unname(slurm_template_opts(tmpl)$value), c("nm1", "4", "proj-x", "01:00:00"))
})

test_that("a \"{{placeholder}}\" argument leaves a blank with that name", {
  tmpl <- Template() |> with_cpus_per_task("{{ncpu}}") |> with_sbatch_flag("mem", "{{ memory }}")
  expect_equal(tmpl@sbatch, c("cpus-per-task" = "ncpu", mem = "memory"))
  expect_equal(tmpl@fills, list())
  expect_equal(format(tmpl)[2:3], c("#SBATCH --cpus-per-task={{ncpu}}", "#SBATCH --mem={{memory}}"))
})

test_that("a value given to a verb is checked like a fill", {
  expect_error(Template() |> with_nodes(0), "positive whole number")
  expect_error(Template() |> with_cpus_per_task("ncpu"), "positive whole number")
  expect_error(Template() |> with_partition(""), "non-empty string")
  expect_error(Template() |> with_nodes(c(1, 2)), "single value")
  expect_error(Template() |> with_nodes(NA), "single value")
})

test_that("a value given again, or filled later, replaces the earlier one", {
  tmpl <- Template() |> with_nodes(1) |> with_nodes(2)
  expect_equal(tmpl@fills, list(nodes = 2))
  expect_equal(fill(tmpl, nodes = 4)@fills, list(nodes = 4))
  # renaming a filled flag's blank orphans the value, and says so
  expect_error(with_nodes(tmpl, "{{n}}"), "does not have")
})

test_that("with_sbatch_flag needs a Template", {
  expect_error(with_sbatch_flag("rscript.tmpl", "partition"), "must be a Template")
})

test_that("Template() validates flags passed directly", {
  expect_error(Template(sbatch = c("a", "b")), "named character vector")
  expect_error(Template(sbatch = c(nodes = "n", nodes = "m")), "once")
})

test_that("print shows the rendered header", {
  expect_output(
    print(Template() |> with_sbatch_flag("partition")),
    "#SBATCH --partition={{partition}}",
    fixed = TRUE
  )
})

# --- fill() -------------------------------------------------------------------

test_that("fill renders a value in place and leaves the rest as placeholders", {
  tmpl <- Template() |>
    with_sbatch_flag("job-name") |>
    with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
    with_sbatch_flag("partition") |>
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
    with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
    with_sbatch_flag("ntasks", "{{ncpu}}") |>
    fill(ncpu = 4)
  expect_equal(
    format(tmpl)[-1],
    c("#SBATCH --cpus-per-task=4", "#SBATCH --ntasks=4")
  )
})

test_that("numbers render as sbatch expects them, never in scientific notation", {
  tmpl <- Template() |> with_sbatch_flag("mem") |> fill(mem = 100000)
  expect_equal(format(tmpl)[2], "#SBATCH --mem=100000")
})

test_that("filling a placeholder again replaces the earlier value", {
  tmpl <- Template() |>
    with_sbatch_flag("partition") |>
    fill(partition = "a") |>
    fill(partition = "b")
  expect_equal(tmpl@fills, list(partition = "b"))
  expect_equal(format(tmpl)[2], "#SBATCH --partition=b")
})

test_that("fill refuses placeholders the template does not have, and lists the real ones", {
  tmpl <- Template() |>
    with_sbatch_flag("partition") |>
    with_sbatch_flag("cpus-per-task", "{{ncpu}}")
  expect_error(fill(tmpl, partiton = "x"), "no placeholder `{{partiton}}`", fixed = TRUE)
  expect_error(fill(tmpl, partiton = "x"), "`partition`, `ncpu`", fixed = TRUE)
  expect_error(fill(Template(), ncpu = 1), "no placeholders yet")
})

test_that("fill needs named, single values", {
  tmpl <- Template() |> with_sbatch_flag("partition")
  expect_error(fill(tmpl, "cpu2mem4gb"), "must be named")
  expect_error(fill(tmpl, partition = c("a", "b")), "single non-missing value")
  expect_error(fill(tmpl, partition = NULL), "single non-missing value")
  expect_error(fill(tmpl, partition = NA), "single non-missing value")
  expect_error(fill("rscript.tmpl", partition = "x"), "must be a Template")
})

test_that("fill with nothing to fill returns the template unchanged", {
  tmpl <- Template() |> with_sbatch_flag("partition")
  expect_identical(fill(tmpl), tmpl)
})

test_that("a fill for a placeholder the template lacks is caught, however it gets there", {
  expect_error(
    Template(sbatch = c(nodes = "nodes"), fills = list(ncpu = 2)),
    "does not have"
  )
  tmpl <- Template() |> with_sbatch_flag("nodes") |> fill(nodes = 2)
  expect_error(with_sbatch_flag(tmpl, "nodes", "{{n_nodes}}"), "does not have")
})

# --- write_slurm_template() ---------------------------------------------------

test_that("write_slurm_template writes the rendered lines, newline-terminated", {
  path <- withr::local_tempfile(fileext = ".tmpl")
  tmpl <- Template() |>
    with_sbatch_flag("job-name") |>
    with_sbatch_flag("partition") |>
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
  tmpl <- Template() |> with_sbatch_flag("partition")

  expect_error(write_slurm_template(tmpl, path), "already exists")
  expect_equal(readLines(path), "keep me")

  suppressMessages(write_slurm_template(tmpl, path, overwrite = TRUE))
  expect_equal(readLines(path)[1], "#!/bin/bash")
})

test_that("write_slurm_template needs a Template", {
  expect_error(write_slurm_template("rscript.tmpl", "out.tmpl"), "must be a Template")
})

test_that("a written template submits through the file form of submit_slurm_job()", {
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  path <- withr::local_tempfile(fileext = ".tmpl")
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  Template() |>
    with_sbatch_flag("job-name") |>
    with_sbatch_flag("partition") |>
    with_sbatch_flag("cpus-per-task", "{{ncpu}}") |>
    write_slurm_template(path) |>
    suppressMessages()
  # the command line is the user's to add; the file is theirs
  cat("echo {{file}}\n", file = path, append = TRUE)

  cmd <- submit_slurm_job(
    script,
    slurm_job_template_path = path,
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
    c("with_sbatch_flag", "with_command", "with_pre_run", "with_post_run") # the verbs that are not a flag
  )
  expect_setequal(verbs, paste0("with_", gsub("-", "_", names(sbatch_flags))))
  for (flag in names(sbatch_flags)) {
    verb <- get(paste0("with_", gsub("-", "_", flag)))
    expect_equal(
      verb(Template())@sbatch,
      stats::setNames(gsub("-", "_", flag), flag),
      info = flag
    )
    expect_equal(verb(Template(), "{{x}}")@sbatch, stats::setNames("x", flag), info = flag)
  }
})

test_that("the meeting's sketch renders through the named verbs", {
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("{{ncpu}}") |>
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
  tmpl <- Template() |> with_cpus_per_task("{{ncpu}}")
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
  tmpl <- Template() |> with_cpus_per_task("{{n}}") |> with_partition("{{n}}")
  expect_error(fill(tmpl, n = 4), "`--partition`", fixed = TRUE)
  expect_error(fill(tmpl, n = "cpu2mem4gb"), "`--cpus-per-task`", fixed = TRUE)
})

test_that("the check follows the flag, not the verb that added it", {
  expect_error(fill(with_sbatch_flag(Template(), "nodes"), nodes = 0), "positive whole number")
  expect_error(
    Template(sbatch = c(nodes = "nodes"), fills = list(nodes = 0)),
    "positive whole number"
  )
  # a flag outside the table is not checked
  expect_equal(
    format(fill(with_sbatch_flag(Template(), "time"), time = "not-a-time"))[2],
    "#SBATCH --time=not-a-time"
  )
})

# --- optional flags -------------------------------------------------------------

test_that("an unfilled optional flag renders inside a section, in its place", {
  tmpl <- Template() |>
    with_partition() |>
    with_account(optional = TRUE) |>
    with_cpus_per_task("{{ncpu}}")
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
    format(with_sbatch_flag(Template(), "time", optional = TRUE))[-1],
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

test_that("a written optional flag is dropped or kept by the file form as today", {
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  path <- withr::local_tempfile(fileext = ".tmpl")
  script <- withr::local_tempfile(fileext = ".R", lines = "print('hi')")

  Template() |>
    with_job_name() |>
    with_partition() |>
    with_account(optional = TRUE) |>
    with_cpus_per_task("{{ncpu}}") |>
    write_slurm_template(path) |>
    suppressMessages()
  cat("echo {{file}}\n", file = path, append = TRUE)

  without <- submit_slurm_job(script, slurm_job_template_path = path, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_no_match(without$template_script, "--account", fixed = TRUE)
  expect_match(without$template_script, "#SBATCH --partition=cpu2mem4gb\n#SBATCH --cpus-per-task=1", fixed = TRUE)

  with <- submit_slurm_job(
    script,
    slurm_job_template_path = path,
    partition = "cpu2mem4gb",
    dry_run = TRUE,
    slurm_template_opts = list(account = "proj-x")
  )
  expect_match(with$template_script, "#SBATCH --account=proj-x", fixed = TRUE)
})

# --- with_command() -----------------------------------------------------------

echo_path <- unname(Sys.which("echo"))

test_that("with_command resolves the program and keeps its args, placeholders included", {
  tmpl <- Template() |> with_command("echo", c("run", "{{file}}"))
  expect_length(tmpl@body, 1)
  expect_equal(tmpl@body[[1]]$exe, echo_path)
  expect_equal(tmpl@body[[1]]$args, c("run", "{{file}}"))
  expect_equal(utils::tail(format(tmpl), 1), paste(echo_path, "run {{file}}"))

  tmpl <- with_command(tmpl, "/opt/tool/bin/tool", "--flag")
  expect_equal(utils::tail(format(tmpl), 2), c(paste(echo_path, "run {{file}}"), "/opt/tool/bin/tool --flag"))

  expect_error(with_command(Template(), "not-a-real-tool"), "could not find")
  expect_error(with_command(Template(), "Rscript sim.R"), "arguments go in `args`")
  expect_error(with_pre_run(Template(), "module load R"), "arguments go in `args`")
  expect_error(with_command(Template(), c("a", "b")), "single program")
  expect_error(with_command(Template(), "echo", 1), "character vector")
  expect_error(with_command("x", "echo"), "must be a Template")
})

test_that("the program and its arguments are never one string: an argument with a space is one word", {
  tmpl <- Template() |> with_command("echo", c("-d", "job {{job_name}} finished", "'already quoted'", "\"and this\""))
  expect_equal(
    utils::tail(format(tmpl), 1),
    paste(echo_path, "-d \"job {{job_name}} finished\" 'already quoted' \"and this\"")
  )
  expect_equal(shell_arg(c("a", "a b", "$X y")), c("a", "\"a b\"", "\"$X y\""))
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
    with_job_name("{{run}}") |>
    with_command("echo", "{{run}}") |>
    fill(run = "1001")
  expect_equal(
    format(tmpl)[c(2, 4)],
    c("#SBATCH --job-name=1001", paste(echo_path, "1001"))
  )
})

test_that("a conditional writes both forms of the line, each in its section", {
  tmpl <- Template() |>
    with_job_name() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command(
      "echo", c("nonmem", "run", "{{file}}"),
      conditional = "parallel", if_true = c("--parallel", "--threads={{ncpu}}"), if_false = "--serial"
    )
  expect_equal(
    utils::tail(format(tmpl), 6),
    c(
      "{{#parallel}}",
      paste(echo_path, "nonmem run {{file}} --parallel --threads={{ncpu}}"),
      "{{/parallel}}",
      "{{^parallel}}",
      paste(echo_path, "nonmem run {{file}} --serial"),
      "{{/parallel}}"
    )
  )
  expect_setequal(template_placeholders(tmpl), c("job_name", "ncpu", "parallel", "file"))

  serial <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "a.mod", ncpu = 1))
  expect_equal(utils::tail(strsplit(serial$template_script, "\n")[[1]], 1), paste(echo_path, "nonmem run a.mod --serial"))
  parallel <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "a.mod", ncpu = 4))
  expect_equal(utils::tail(strsplit(parallel$template_script, "\n")[[1]], 1), paste(echo_path, "nonmem run a.mod --parallel --threads=4"))
  expect_no_match(parallel$template_script, "--serial", fixed = TRUE)
})

test_that("if_false defaults to the plain line, and the conditional arguments are checked", {
  tmpl <- Template() |> with_command("echo", "x", conditional = "parallel", if_true = "--par")
  expect_equal(format(tmpl)[7], paste(echo_path, "x"))
  expect_error(with_command(Template(), "echo", "x", if_true = "--par"), "need a `conditional`")
  expect_error(with_command(Template(), "echo", "x", conditional = c("a", "b")), "single placeholder name")
  expect_error(with_command(Template(), "echo", "x", conditional = "2x"), "single placeholder name")
  expect_error(with_command(Template(), "echo", "x", conditional = "p", if_true = 1), "`if_true` must be")
})

test_that("a program given as a placeholder is filled like any other and resolved at submit", {
  tmpl <- Template() |> with_job_name() |> with_command("{{tool}}", "{{file}}")
  expect_equal(utils::tail(format(tmpl), 1), "{{tool}} {{file}}")
  expect_setequal(template_placeholders(tmpl), c("job_name", "tool", "file"))

  # a bare name is looked up on the PATH once filled; a path is taken as given
  dry <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", tool = "echo"))
  expect_equal(utils::tail(strsplit(dry$template_script, "\n")[[1]], 1), paste(echo_path, "sim.R"))
  dry <- submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", tool = "/opt/x/echo"))
  expect_equal(utils::tail(strsplit(dry$template_script, "\n")[[1]], 1), "/opt/x/echo sim.R")
  expect_error(
    submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", tool = "no-such-tool-xyz")),
    "could not find `no-such-tool-xyz`"
  )
})

# --- submit_slurm_job(<Template>) ---------------------------------------------

sketch <- function() {
  Template() |>
    with_job_name() |>
    with_partition() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_account(optional = TRUE) |>
    with_command("echo", "{{file}}")
}

test_that("a Template submits with slurm_template_opts as the final fill (dry run)", {
  root <- withr::local_tempdir()
  withr::local_options(slurmtools.submission_root = root)
  cmd <- submit_slurm_job(
    sketch(),
    partition = "cpu2mem4gb",
    ncpu = 2,
    dry_run = TRUE,
    slurm_template_opts = list(file = "sim.R", job_name = "sim")
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

test_that("main's positional order (.mod, partition, ncpu, overwrite, dry_run) binds for a Template", {
  cmd <- submit_slurm_job(sketch(), "cpu4mem32gb", 4, FALSE, TRUE, slurm_template_opts = list(file = "sim.R"))
  expect_match(cmd$template_script, "#SBATCH --partition=cpu4mem32gb", fixed = TRUE)
  expect_match(cmd$template_script, "#SBATCH --cpus-per-task=4", fixed = TRUE)
  expect_equal(cmd$partition, "cpu4mem32gb")
})

test_that("a template's own --output / --error are kept, each defaulted on its own", {
  root <- withr::local_tempdir()
  withr::local_options(slurmtools.submission_root = root)
  tmpl <- sketch() |> with_output("{{log_path}}")
  cmd <- submit_slurm_job(
    tmpl,
    partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE,
    slurm_template_opts = list(file = "s", job_name = "s", log_path = "mine-%j.log")
  )
  expect_match(cmd$template_script, "#SBATCH --output=mine-%j.log", fixed = TRUE)
  expect_no_match(cmd$template_script, "s-%j.out", fixed = TRUE)
  expect_match(cmd$template_script, sprintf("#SBATCH --error=%s/s-%%j.err", root), fixed = TRUE)
})

test_that("pre-filled and submit-time fills combine; submit-time wins", {
  tmpl <- sketch() |> fill(job_name = "a", ncpu = 1)
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 2, dry_run = TRUE, slurm_template_opts = list(file = "sim.R"))
  expect_match(cmd$template_script, "--cpus-per-task=2", fixed = TRUE)
  expect_match(cmd$template_script, "--job-name=a", fixed = TRUE)
})

test_that("unfilled required placeholders stop the submit; unfilled optional ones are dropped", {
  expect_error(
    submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE, slurm_template_opts = list(job_name = "sim")),
    "unfilled placeholders: `{{file}}`",
    fixed = TRUE
  )
  expect_error(
    submit_slurm_job(sketch(), dry_run = TRUE, slurm_template_opts = list(job_name = "sim")),
    "slurm_template_opts = list(file = ...)",
    fixed = TRUE
  )
  expect_error(
    # partition and ncpu fall back to their defaults; the other two do not
    submit_slurm_job(sketch(), dry_run = TRUE),
    "`{{job_name}}`, `{{file}}`",
    fixed = TRUE
  )
  cmd <- submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, dry_run = TRUE, slurm_template_opts = list(file = "s", job_name = "s"))
  expect_no_match(cmd$template_script, "account", fixed = TRUE)
  cmd <- submit_slurm_job(
    sketch(),
    partition = "cpu2mem4gb",
    ncpu = 1,
    dry_run = TRUE,
    slurm_template_opts = list(file = "s", job_name = "s", account = "proj-x")
  )
  expect_match(cmd$template_script, "#SBATCH --account=proj-x", fixed = TRUE)
})

test_that("a template with no command is refused", {
  tmpl <- Template() |> with_partition()
  expect_error(submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE), "no command")
})

test_that("file-form and NONMEM-form arguments are refused with a Template", {
  expect_error(
    submit_slurm_job(sketch(), slurm_job_template_path = "x.tmpl", dry_run = TRUE, slurm_template_opts = list(file = "s")),
    "file form"
  )
  expect_error(
    submit_slurm_job(sketch(), overwrite = TRUE, dry_run = TRUE, slurm_template_opts = list(file = "s")),
    "`overwrite` belongs to the pre-template NONMEM form"
  )
  expect_error(submit_slurm_job(sketch(), dry_run = TRUE, slurm_template_opts = list("s")), "must be a named list")
  expect_error(submit_slurm_job(sketch(), dry_run = TRUE, slurm_template_opts = c(file = "s")), "must be a named list")
})

test_that("ncpu goes to a {{ncpu}} placeholder, or fails naming the real ones", {
  tmpl <- Template() |> with_partition() |> with_cpus_per_task() |> with_command("echo", "hi")
  expect_error(
    submit_slurm_job(tmpl, partition = "cpu2mem4gb", ncpu = 2, dry_run = TRUE),
    "no placeholder `{{ncpu}}`",
    fixed = TRUE
  )
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE, slurm_template_opts = list(cpus_per_task = 2))
  expect_match(cmd$template_script, "--cpus-per-task=2", fixed = TRUE)
})

test_that("the partition is validated and cpus checked against it, as for files", {
  expect_error(
    submit_slurm_job(sketch(), partition = "not-a-partition", ncpu = 1, dry_run = TRUE, slurm_template_opts = list(file = "s", job_name = "s")),
    "not an available partition"
  )
  expect_error(
    submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 64, dry_run = TRUE, slurm_template_opts = list(file = "s", job_name = "s")),
    "greater than number of available CPUs"
  )
})

# a stub sbatch on the PATH that answers `--parsable` style, so nothing is submitted
stub_sbatch <- function(says, env = parent.frame(), exit = 0L) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  brio::write_file(sprintf("#!/bin/bash\necho '%s'\nexit %d\n", says, exit), file.path(stub_dir, "sbatch"))
  fs::file_chmod(file.path(stub_dir, "sbatch"), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
}

test_that("a real submit writes <job_name>.sh, calls sbatch --parsable and returns sbatch's result plus the id and paths", {
  root <- withr::local_tempdir()
  stub_sbatch("12345")

  job <- submit_slurm_job(
    sketch(),
    partition = "cpu2mem4gb",
    ncpu = 1,
    submission_root = root,
    slurm_template_opts = list(file = "sim.R", job_name = "sim")
  )

  # main's return, processx::run()'s list, with the job's whereabouts added
  expect_type(job, "list")
  expect_false(S7::S7_inherits(job))
  expect_equal(job$status, 0L)
  expect_match(job$stdout, "12345")
  expect_equal(job$job_id, "12345")
  expect_equal(job$job_name, "sim")
  expect_equal(job$partition, "cpu2mem4gb")
  expect_equal(job$script, file.path(root, "sim.sh"))
  expect_equal(job$output, file.path(root, "sim-12345.out"))
  expect_equal(job$error, file.path(root, "sim-12345.err"))

  script <- brio::read_file(job$script)
  expect_match(script, "#SBATCH --job-name=sim", fixed = TRUE)
  expect_match(script, sprintf("#SBATCH --output=%s/sim-%%j.out", root), fixed = TRUE)
  expect_match(script, sprintf("#SBATCH --error=%s/sim-%%j.err", root), fixed = TRUE)
})

test_that("the job id is read off sbatch --parsable output, cluster suffix and all", {
  root <- withr::local_tempdir()
  stub_sbatch("77;cluster1")
  job <- submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, submission_root = root, slurm_template_opts = list(file = "s", job_name = "s"))
  expect_equal(job$job_id, "77")
  expect_equal(job$output, file.path(root, "s-77.out"))
})

test_that("an sbatch that returns no id is reported, not swallowed", {
  root <- withr::local_tempdir()
  stub_sbatch("Submitted batch job 9")
  expect_error(
    submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, submission_root = root, slurm_template_opts = list(file = "s", job_name = "s")),
    "did not return a job id"
  )
})

test_that("`...` goes to processx::run(), as it always did", {
  root <- withr::local_tempdir()
  stub_sbatch("", exit = 1L)
  # processx throws on a non-zero exit unless told otherwise; told otherwise, our own check speaks
  expect_error(
    submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, submission_root = root, slurm_template_opts = list(file = "s", job_name = "s")),
    "System command 'sbatch' failed"
  )
  expect_error(
    submit_slurm_job(sketch(), partition = "cpu2mem4gb", ncpu = 1, submission_root = root, slurm_template_opts = list(file = "s", job_name = "s"), error_on_status = FALSE),
    "did not return a job id"
  )
})

test_that("a template's own log path lands in the result with %j resolved", {
  root <- withr::local_tempdir()
  stub_sbatch("5")
  tmpl <- sketch() |> with_output("{{log_path}}")
  job <- submit_slurm_job(
    tmpl,
    partition = "cpu2mem4gb", ncpu = 1, submission_root = root,
    slurm_template_opts = list(file = "s", job_name = "s", log_path = "mine-%j.log")
  )
  expect_equal(job$output, "mine-5.log")
  expect_equal(job$error, file.path(root, "s-5.err"))
})

test_that("without a job-name flag the script is slurm-job.sh", {
  tmpl <- Template() |> with_partition() |> with_command("echo", "hi")
  cmd <- submit_slurm_job(tmpl, partition = "cpu2mem4gb", dry_run = TRUE)
  expect_equal(basename(cmd$args[[2]]), "slurm-job.sh")
  expect_match(cmd$template_script, "slurm-job-%j.out", fixed = TRUE)
})

# --- slurm_template_opts() ------------------------------------------------------

test_that("slurm_template_opts() lists every placeholder with what it feeds and what it falls back to", {
  tmpl <- default_template("echo", c("{{file}}", "--config", "{{config}}"), conditional = "parallel", if_true = "--par") |>
    with_post_run("echo", "{{file_stem}}")
  opts <- slurm_template_opts(tmpl)
  expect_s3_class(opts, "tbl_df")
  expect_equal(names(opts), c("option", "used_by", "required", "value", "default"))
  row <- function(name) opts[opts$option == name, ]
  expect_equal(row("job_name")$used_by, "--job-name")
  expect_equal(row("job_name")$default, "the stem of `file`")
  expect_equal(row("nodes")$value, "1")
  expect_equal(row("ncpu")$default, "1")
  expect_equal(row("partition")$default, "the first partition")
  expect_false(row("account")$required)
  expect_equal(row("account")$default, "left out")
  expect_equal(row("config")$used_by, "script")
  expect_true(row("config")$required)
  expect_true(is.na(row("config")$default))
  expect_false(row("parallel")$required)
  expect_false(row("file_stem")$required)
  expect_equal(row("file_stem")$default, "derived from `file`")
  expect_true(row("file")$required)
  expect_error(slurm_template_opts("x"), "must be a Template")
})
