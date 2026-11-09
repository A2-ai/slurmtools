# one renderer for print, write and submit (deep-review finding 3), and shell
# quoting of filled values once they are in place (finding 6). Dry runs only.

echo_bin <- unname(Sys.which("echo"))

a_template <- function(...) {
  Template() |>
    with_job_name() |>
    with_partition() |>
    with_cpus_per_task("{{ncpu}}") |>
    with_command("echo", c(...))
}

command_line <- function(tmpl) utils::tail(format(tmpl), 1)

dry_run <- function(tmpl, ...) {
  local_partition_table()
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  submit_slurm_job(tmpl, dry_run = TRUE, slurm_template_opts = list(...))
}

test_that("a filled path with a space is one word to the program", {
  tmpl <- a_template("{{file}}") |> fill(file = "my model/sim.R")
  expect_equal(command_line(tmpl), paste(echo_bin, "'my model/sim.R'"))
})

test_that("a filled value with ; or $ reaches the program as typed; a literal $ in the template still expands", {
  tmpl <- a_template("{{file}}", "{{out}}", "literal-$SLURM_JOB_ID")
  expect_equal(
    command_line(fill(tmpl, file = "sim.R", out = "x; echo INJECTED")),
    paste(echo_bin, "sim.R 'x; echo INJECTED' literal-$SLURM_JOB_ID")
  )
  expect_equal(
    command_line(fill(tmpl, file = "sim.R", out = "fill-$SLURM_JOB_ID")),
    paste(echo_bin, "sim.R 'fill-$SLURM_JOB_ID' literal-$SLURM_JOB_ID")
  )
})

test_that("plain filled values are not quoted, so the usual script reads as before", {
  tmpl <- a_template("{{file}}", "--seed={{seed}}") |> fill(file = "sim.R", seed = 1)
  expect_equal(command_line(tmpl), paste(echo_bin, "sim.R --seed=1"))
})

test_that("a placeholder inside the author's own text keeps the literal rule: double quotes only for whitespace", {
  tmpl <- a_template("--out={{out}}", "{{file_dir}}/{{file_stem}}.lst", "{{file_stem}}/worker*", "{{file_stem}} done $SLURM_JOB_ID") |>
    fill(out = "my dir", file = "my model/run1.mod")
  expect_equal(
    command_line(tmpl),
    paste(echo_bin, "\"--out=my dir\" \"my model/run1.lst\" run1/worker* \"run1 done $SLURM_JOB_ID\"")
  )
})

test_that("a placeholder the author quoted themselves is left alone", {
  tmpl <- a_template("\"{{file}}\"") |> fill(file = "my model/sim.R")
  expect_equal(command_line(tmpl), paste(echo_bin, "\"my model/sim.R\""))
})

test_that("a program given as a placeholder is filled, and quoted if its path needs it", {
  rscript <- Template() |> with_command("{{prog}}", "{{file}}") |> fill(prog = "/opt/my tools/Rscript", file = "sim.R")
  expect_equal(command_line(rscript), "'/opt/my tools/Rscript' sim.R")
})

test_that("quote characters inside a value are data, not the author's quoting; an empty value is still an argument", {
  tmpl <- a_template("{{note}}", "end")
  expect_equal(command_line(fill(tmpl, note = "\"$HOME\"")), paste(echo_bin, "'\"$HOME\"' end"))
  expect_equal(command_line(fill(tmpl, note = "\"$(touch /tmp/marker)\"")), paste(echo_bin, "'\"$(touch /tmp/marker)\"' end"))
  expect_equal(command_line(fill(tmpl, note = "")), paste(echo_bin, "'' end"))
})

test_that("a value that looks like a tag is pasted in as text, never read as a placeholder", {
  tmpl <- a_template("{{note}}", "{{file}}")
  filled <- fill(tmpl, note = "{{other}}", file = "sim.R")
  expect_equal(command_line(filled), paste(echo_bin, "'{{other}}' sim.R"))
  expect_setequal(required_placeholders(filled), c("job_name", "partition", "ncpu", "note", "file"))
  d <- dry_run(filled, partition = "cpu2mem4gb", ncpu = 1)
  expect_match(d$template_script, "'{{other}}' sim.R", fixed = TRUE)
})

test_that("a value that contains a section tag, or a space before one, is pasted in untouched", {
  # two open conditionals render as inline sections, each with no gap before it
  tmpl <- Template() |> with_job_name("x") |>
    with_command("echo", c("{{note}}", "run"), conditional("parallel", if_true = "--par"), conditional("verbose", if_true = "-v"))
  filled <- fill(tmpl, note = "a {{#other}}")
  expect_equal(
    command_line(filled),
    paste0(echo_bin, " 'a {{#other}}' run{{#parallel}} --par{{/parallel}}{{#verbose}} -v{{/verbose}}")
  )
  expect_equal(command_line(fill(filled, parallel = TRUE, verbose = FALSE)), paste(echo_bin, "'a {{#other}}' run --par"))
})

test_that("a hand-written {{#section}} in an argument is refused at build time, with conditional() suggested", {
  expect_error(
    Template() |> with_command("echo", "{{#enabled}}yes{{/enabled}}"),
    "not a placeholder"
  )
  expect_error(
    Template() |> with_command("echo", "{{#enabled}}yes{{/enabled}}"),
    "conditional(", fixed = TRUE
  )
  expect_error(Template() |> with_post_run("echo", "{{^x}}no{{/x}}"), "not a placeholder")
})

test_that("a value with an apostrophe is still one word", {
  skip_on_os("windows")
  tmpl <- a_template("{{note}}") |> fill(note = "it's done")
  line <- command_line(tmpl)
  expect_match(line, "it's done", fixed = TRUE)
  # bash would read it back as one argument
  words <- sub(paste0("^", echo_bin, " "), "", line)
  expect_equal(system2("bash", c("-c", shQuote(paste("printf '%s\\n'", words))), stdout = TRUE), "it's done")
})

test_that("an unfilled template prints and writes as before: literals with whitespace in double quotes", {
  tmpl <- a_template("-d", "job {{job_name}} finished", "{{file}}")
  expect_equal(command_line(tmpl), paste(echo_bin, "-d \"job {{job_name}} finished\" {{file}}"))
  expect_equal(
    command_line(fill(tmpl, job_name = "my model", file = "sim.R")),
    paste(echo_bin, "-d \"job my model finished\" sim.R")
  )
})

test_that("spaces inside the braces fill the same way in print and at submit", {
  tmpl <- a_template("{{ file }}", "{{note}}") |> fill(file = "args.R", note = "a&b")
  expect_equal(command_line(tmpl), paste(echo_bin, "args.R 'a&b'"))
  d <- dry_run(tmpl, partition = "cpu2mem4gb", ncpu = 1)
  expect_equal(utils::tail(strsplit(d$template_script, "\n")[[1]], 1), command_line(tmpl))
})

test_that("the submitted script is print()'s script plus the default log lines, with no tags left", {
  tmpl <- a_template("{{file}}") |> with_account(optional = TRUE)
  d <- dry_run(tmpl, file = "sim.R", partition = "cpu2mem4gb", ncpu = 1)
  script <- strsplit(d$template_script, "\n")[[1]]
  preview <- format(fill(tmpl, file = "sim.R", job_name = "sim", partition = "cpu2mem4gb", ncpu = 1))
  # line for line, in order: the script is the preview minus the unfilled
  # optional flag's {{#account}} section (dropped at the final render instead
  # of left to whisker) plus the two default log lines
  account_section <- c("{{#account}}", "#SBATCH --account={{account}}", "{{/account}}")
  expect_equal(
    script[!grepl("^#SBATCH --(output|error)=", script)],
    preview[!preview %in% account_section]
  )
  expect_false(any(grepl("{{", script, fixed = TRUE)))
  expect_equal(sum(grepl("^#SBATCH --(output|error)=", script)), 2)
})

test_that("a header value with both whitespace and a double quote is refused when set, so print() never fails", {
  expect_error(Template() |> with_sbatch_flag("comment", "a \"b c\""), "both whitespace and a double quote")
  expect_error(Template() |> with_sbatch_flag("comment", "\"a \"b c\"\""), "both whitespace and a double quote")
  expect_error(fill(Template() |> with_job_name(), job_name = "my \"model\""), "both whitespace and a double quote")
  # whitespace alone, a quote alone, or one pair of quotes the author wrote, are fine
  expect_true("#SBATCH --comment=\"a b\"" %in% format(Template() |> with_sbatch_flag("comment", "a b")))
  expect_true("#SBATCH --comment=a\"b" %in% format(Template() |> with_sbatch_flag("comment", "a\"b")))
  expect_true("#SBATCH --comment=\"a b\"" %in% format(Template() |> with_sbatch_flag("comment", "\"a b\"")))
})

test_that("a header value with a space is double-quoted, including the default log paths", {
  d <- dry_run(a_template("{{file}}"), file = "sim.R", job_name = "my model", partition = "cpu2mem4gb", ncpu = 1)
  script <- strsplit(d$template_script, "\n")[[1]]
  expect_true("#SBATCH --job-name=\"my model\"" %in% script)
  expect_true(any(grepl("^#SBATCH --output=\".*/my model-%j\\.out\"$", script)))
  expect_true(any(grepl("^#SBATCH --error=\".*/my model-%j\\.err\"$", script)))
  expect_true("#SBATCH --partition=cpu2mem4gb" %in% script)
})

test_that("{{log_dir}} stays a tag in the preview and is filled at submit", {
  tmpl <- a_template("{{file}}", "{{log_dir}}")
  expect_match(command_line(fill(tmpl, file = "sim.R")), "{{log_dir}}", fixed = TRUE)
  d <- dry_run(tmpl, file = "sim.R", partition = "cpu2mem4gb", ncpu = 1)
  last <- utils::tail(strsplit(d$template_script, "\n")[[1]], 1)
  expect_false(grepl("{{", last, fixed = TRUE))
  expect_match(last, paste0("^", echo_bin, " sim.R .+"))
})

test_that("the file form no longer HTML-escapes values", {
  local_partition_table()
  root <- withr::local_tempdir()
  withr::local_options(slurmtools.submission_root = root)
  path <- file.path(root, "rs.tmpl")
  suppressMessages(write_slurm_template(a_template("{{file}}", "{{note}}"), path))
  script <- file.path(root, "args.R")
  writeLines("", script)
  d <- submit_slurm_job(script, dry_run = TRUE, slurm_job_template_path = path,
                        partition = "cpu2mem4gb", ncpu = 1, slurm_template_opts = list(note = "a&b <c>"))
  expect_match(d$template_script, paste(echo_bin, script, "a&b <c>"), fixed = TRUE)
  expect_no_match(d$template_script, "&amp;", fixed = TRUE)
})

test_that("unescape_tags() widens plain tags only", {
  expect_equal(unescape_tags("{{a}} {{ b }} {{#c}}{{d}}{{/c}} {{{e}}}"), "{{{a}}} {{{b}}} {{#c}}{{{d}}}{{/c}} {{{e}}}")
})

test_that("a script with no --job-name directive is named after its script, not the temporary file", {
  local_partition_table()
  withr::local_options(slurmtools.submission_root = withr::local_tempdir())
  bare <- Template() |> with_partition() |> with_command("echo", "{{file}}")
  d <- submit_slurm_job(bare, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", partition = "cpu2mem4gb"))
  expect_equal(d$args[1:2], c("--parsable", "--job-name=slurm-job"))
  # one that says so itself is left alone
  named <- a_template("{{file}}")
  d <- submit_slurm_job(named, dry_run = TRUE, slurm_template_opts = list(file = "sim.R", partition = "cpu2mem4gb", ncpu = 1))
  expect_equal(d$args[[1]], "--parsable")
  expect_false(any(grepl("^--job-name", d$args)))
})
