a_job <- function(id = "2051") {
  Job(
    job_id = id,
    job_name = "sim",
    partition = "cpu2mem4gb",
    script = "submission-log/sim.sh",
    output = sprintf("submission-log/sim-%s.out", id),
    error = sprintf("submission-log/sim-%s.err", id),
    sbatch = list()
  )
}

# a stub sacct on the PATH: records the arguments it was called with, prints `says`
stub_sacct <- function(says, env = parent.frame()) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  args_file <- file.path(stub_dir, "args")
  brio::write_file(
    sprintf("#!/bin/bash\necho \"$@\" > '%s'\nprintf '%%s\\n' %s\n", args_file, shQuote(says)),
    file.path(stub_dir, "sacct")
  )
  fs::file_chmod(file.path(stub_dir, "sacct"), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
  args_file
}

row_2051 <- "2051|sim|cpu2mem4gb|COMPLETED|0:0|00:00:11|cpu2mem4gb-dy-c7i-large-1|2026-09-15T19:53:47|2026-09-15T19:58:07|2026-09-15T19:58:18"

# --- slurm_job_id() -----------------------------------------------------------

test_that("slurm_job_id takes a Job or ids, nothing else", {
  expect_equal(slurm_job_id(a_job()), "2051")
  expect_equal(slurm_job_id(2051), "2051")
  expect_equal(slurm_job_id(c("2051", "2052")), c("2051", "2052"))
  expect_error(slurm_job_id(list(1)), "expected the Job")
  expect_error(slurm_job_id(NULL), "expected the Job")
  expect_error(slurm_job_id(NA), "expected the Job")
})

# --- slurm_job_status() -------------------------------------------------------

test_that("slurm_job_status asks sacct for the job's allocation row and tidies it", {
  args_file <- stub_sacct(row_2051)

  status <- slurm_job_status(a_job())

  expect_match(readLines(args_file), "-j 2051 --allocations --parsable2 --noheader --format=JobID,JobName,Partition,State,ExitCode,Elapsed,NodeList,Submit,Start,End", fixed = TRUE)
  expect_s3_class(status, "tbl_df")
  expect_equal(nrow(status), 1)
  expect_equal(
    names(status),
    c("job_id", "job_name", "partition", "state", "exit_code", "elapsed", "node", "submit", "start", "end")
  )
  expect_equal(status$job_id, "2051")
  expect_equal(status$state, "COMPLETED")
  expect_equal(status$exit_code, "0:0")
  expect_equal(status$node, "cpu2mem4gb-dy-c7i-large-1")
  expect_s3_class(status$submit, "POSIXct")
  expect_equal(format(status$end, "%H:%M:%S"), "19:58:18")
})

test_that("several ids go in one sacct call and come back one row each", {
  args_file <- stub_sacct(paste(row_2051, sub("^2051", "2052", row_2051), sep = "\n"))
  status <- slurm_job_status(c(2051, 2052))
  expect_match(readLines(args_file), "-j 2051,2052 ", fixed = TRUE)
  expect_equal(status$job_id, c("2051", "2052"))
})

test_that("the 'by <uid>' suffix is dropped from CANCELLED, and unknown times are NA", {
  stub_sacct("2060|sleep|cpu2mem4gb|CANCELLED by 603603109|0:0|00:00:00|None assigned|2026-09-15T20:00:00|Unknown|Unknown")
  status <- slurm_job_status(2060)
  expect_equal(status$state, "CANCELLED")
  expect_true(is.na(status$start))
  expect_true(is.na(status$end))
  expect_false(is.na(status$submit))
})

test_that("a job sacct does not know gives an empty table with the same columns", {
  stub_sacct("")
  status <- slurm_job_status(99999999)
  expect_equal(nrow(status), 0)
  expect_equal(names(status)[c(1, 4, 10)], c("job_id", "state", "end"))
  expect_s3_class(status$submit, "POSIXct")
})

test_that("a missing sacct is reported", {
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_error(slurm_job_status(a_job()), "could not find sacct")
})

# --- cancel_slurm_job(<Job>) --------------------------------------------------

test_that("cancel_slurm_job resolves a Job to its id before looking it up", {
  # an id no queue holds: the lookup fails the same way it does for a bare id,
  # which is the point — the handle was accepted where an id was
  expect_error(
    capture.output(suppressMessages(
      cancel_slurm_job(a_job("999999999"), auto_confirm = TRUE)
    )),
    "associated your user_name"
  )
  expect_error(cancel_slurm_job(list(1)), "expected the Job")
})

# --- print(<Job>) points at the lifecycle functions ---------------------------

test_that("printing a Job names what to do with it next", {
  expect_output(print(a_job()), "slurm_job_status(job)", fixed = TRUE)
  expect_output(print(a_job()), "cancel_slurm_job(job)", fixed = TRUE)
})

test_that("printing a Job also names wait_for_slurm_job() and slurm_job_log()", {
  expect_output(print(a_job()), "wait_for_slurm_job(job)", fixed = TRUE)
  expect_output(print(a_job()), "slurm_job_log(job)", fixed = TRUE)
})
