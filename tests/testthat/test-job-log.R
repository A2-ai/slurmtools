# --- slurm_job_log() ----------------------------------------------------------

# a submit result whose logs live in a scratch directory
a_logged_job <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  a_job(output = file.path(dir, "sim-2051.out"), error = file.path(dir, "sim-2051.err"))
}

test_that("slurm_job_log reads the output log by default and the error log on request", {
  job <- a_logged_job()
  writeLines(c("[1] 0.0003", "done"), job$output)
  writeLines("Warning message: x", job$error)

  expect_equal(slurm_job_log(job), c("[1] 0.0003", "done"))
  expect_equal(slurm_job_log(job, "error"), "Warning message: x")
  expect_equal(slurm_job_log(job, which = "output"), c("[1] 0.0003", "done"))
})

test_that("n keeps the last n lines", {
  job <- a_logged_job()
  writeLines(as.character(1:10), job$output)
  expect_equal(slurm_job_log(job, n = 3), c("8", "9", "10"))
  expect_equal(slurm_job_log(job, n = 100), as.character(1:10))
  expect_equal(slurm_job_log(job, n = 0), character())
})

test_that("an empty log is an empty vector, not an error", {
  job <- a_logged_job()
  file.create(job$error)
  expect_equal(slurm_job_log(job, "error"), character())
})

test_that("before the job has written its log, that is a clear error", {
  job <- a_logged_job()
  expect_error(slurm_job_log(job), "no output log yet at")
  expect_error(slurm_job_log(job), "slurm_job_status(job)", fixed = TRUE)
  expect_error(slurm_job_log(job, "error"), "no error log yet at")
})

test_that("a log the result does not know about is reported as such", {
  job <- a_job(output = NA_character_)
  expect_error(slurm_job_log(job), "left to slurm")
  expect_error(slurm_job_log(job), "slurm-2051.out", fixed = TRUE)
})

test_that("slurm_job_log needs the submit result and checks its arguments", {
  expect_error(slurm_job_log(2051), "must be the result of submit_slurm_job()", fixed = TRUE)
  expect_error(slurm_job_log(list(status = 0L)), "must be the result of submit_slurm_job()", fixed = TRUE)
  job <- a_logged_job()
  writeLines("x", job$output)
  expect_error(slurm_job_log(job, "stdout"), "must be one of")
  expect_error(slurm_job_log(job, n = -1), "`n` must be a non-negative number")
  expect_error(slurm_job_log(job, n = "3"), "`n` must be a non-negative number")
})
