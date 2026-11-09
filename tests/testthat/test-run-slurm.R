# run_slurm(): every Slurm command goes through it, so a missing or failing one
# is reported in the command's own words (deep-review finding 8)

a_stub <- function(name, script, env = parent.frame()) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  brio::write_file(paste0("#!/bin/bash\n", script, "\n"), file.path(stub_dir, name))
  fs::file_chmod(file.path(stub_dir, name), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
}

test_that("a command that works returns processx's result", {
  a_stub("sbatch", "echo \"$@\"")
  res <- run_slurm("sbatch", c("--parsable", "x.sh"))
  expect_equal(res$status, 0L)
  expect_equal(res$stdout, "--parsable x.sh\n")
})

test_that("a missing command is named, and where slurmtools expects to run", {
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_error(run_slurm("sbatch"), "could not find sbatch on the PATH")
  expect_error(run_slurm("sbatch"), "login node")
})

test_that("a failing command is reported in its own words, with the result on the condition", {
  a_stub("sbatch", "echo 'sbatch: error: Invalid directive found in batch script: model' >&2\nexit 1")
  err <- tryCatch(run_slurm("sbatch", "x.sh"), error = function(e) e)
  expect_s3_class(err, "slurmtools_command_error")
  expect_match(conditionMessage(err), "sbatch failed (exit status 1)", fixed = TRUE)
  expect_match(conditionMessage(err), "Invalid directive found in batch script: model", fixed = TRUE)
  expect_equal(err$status, 1L)
  expect_match(err$stderr, "Invalid directive", fixed = TRUE)
})

test_that("error_on_status = FALSE still hands back the failed result, as processx did", {
  a_stub("sbatch", "echo nope >&2\nexit 1")
  res <- run_slurm("sbatch", "x.sh", error_on_status = FALSE)
  expect_equal(res$status, 1L)
  expect_equal(res$stderr, "nope\n")
})
