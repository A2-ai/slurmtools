# --- wait_for_slurm_job() -----------------------------------------------------

test_that("wait_for_slurm_job polls until the job is over and returns that row", {
  calls <- stub_sacct_sequence(c("", sacct_row("PENDING"), sacct_row("RUNNING"), sacct_row("COMPLETED")))

  status <- suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01))

  expect_equal(sacct_calls(calls), 4)
  expect_s3_class(status, "tbl_df")
  expect_equal(nrow(status), 1)
  expect_equal(status$state, "COMPLETED")
  expect_equal(status$exit_code, "0:0")
})

test_that("the final row comes back invisibly", {
  stub_sacct_sequence(sacct_row("COMPLETED"))
  expect_invisible(suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01)))
})

test_that("a failed job is returned, not thrown", {
  stub_sacct_sequence(c(sacct_row("RUNNING"), sacct_row("FAILED", exit_code = "1:0")))
  status <- suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01))
  expect_equal(status$state, "FAILED")
  expect_equal(status$exit_code, "1:0")
})

test_that("every terminal state stops the wait after one look", {
  for (state in c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "OUT_OF_MEMORY",
                  "NODE_FAIL", "PREEMPTED", "BOOT_FAIL", "DEADLINE")) {
    calls <- stub_sacct_sequence(sacct_row(state))
    status <- suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01))
    expect_equal(status$state, state)
    expect_equal(sacct_calls(calls), 1)
  }
})

test_that("a bare id is accepted and asked for by id", {
  stub_sacct_sequence(sacct_row("COMPLETED", id = "2060"))
  status <- suppressMessages(wait_for_slurm_job(2060, poll = 0.01))
  expect_equal(status$job_id, "2060")
})

test_that("a job still running at the deadline is a timeout naming the last state", {
  stub_sacct_sequence(sacct_row("RUNNING"))
  expect_error(
    suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01, timeout = 0.05)),
    "timed out after 0.05 s waiting for job 2051"
  )
  expect_error(
    suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01, timeout = 0.05)),
    "last state: RUNNING"
  )
})

test_that("a job sacct never learns about is polled, then times out as 'not in sacct yet'", {
  calls <- stub_sacct_sequence("")
  expect_error(
    suppressMessages(wait_for_slurm_job(a_job(), poll = 0.01, timeout = 0.05)),
    "last state: not in sacct yet"
  )
  expect_gt(sacct_calls(calls), 1)
})

test_that("wait_for_slurm_job checks its arguments", {
  expect_error(wait_for_slurm_job(c(2051, 2052)), "waits for one job")
  expect_error(wait_for_slurm_job(list(1)), "expected the Job")
  expect_error(wait_for_slurm_job(a_job(), poll = 0), "`poll` must be a positive number")
  expect_error(wait_for_slurm_job(a_job(), poll = "10"), "`poll` must be a positive number")
  expect_error(wait_for_slurm_job(a_job(), timeout = 0), "`timeout` must be a positive number")
  expect_error(wait_for_slurm_job(a_job(), timeout = NA), "`timeout` must be a positive number")
})
