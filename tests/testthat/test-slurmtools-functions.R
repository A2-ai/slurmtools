test_that("get_slurm_partitions returns list of partitions", {
  vectorOfPartitions <- c(
    "cpu2mem4gb",
    "cpu4mem32gb",
    "cpu8mem64gb",
    "cpu2mem8gb",
    "cpu4mem16gb",
    "cpu16mem128gb",
    "cpu8mem32gb",
    "cpu16mem64gb",
    "cpu32mem128gb"
  )

  expect_contains(get_slurm_partitions(), vectorOfPartitions)
})

test_that("check_slurm_partitions errors when 17 cpus are requested for cpu2mem4gb", {
  message <- "number of requested CPUs (17) greater than number of available CPUs in cpu2mem4gb (2)\nYou might try cpu32mem128gb"
  expect_error(check_slurm_partitions(17, "cpu2mem4gb"), message, fixed = TRUE)
})

test_that("check_slurm_partitions errors when 100 cpus are requested for cpu32mem128gb", {
  message <- "number of requested CPUs (100) greater than number of available CPUs in cpu32mem128gb (32)\nInput a smaller value for ncpu. No existing partition has 100 or more CPUs per node."
  expect_error(
    check_slurm_partitions(100, "cpu32mem128gb"),
    message,
    fixed = TRUE
  )
})

test_that("check_slurm_partitions doesn't error when 2 cpus are requested for cpu2mem4gb", {
  expect_no_error(check_slurm_partitions(2, "cpu2mem4gb"))
})

test_that("check_slurm_partitions warns with a smaller partition suggestion when underutilized", {
  message <- paste(
    "number of requested CPUs (3) less than 50% of available CPUs in cpu8mem64gb (8)",
    "Consider increasing `ncpu` or using a smaller partition",
    sep = "\n"
  )

  expect_warning(
    check_slurm_partitions(3, "cpu8mem64gb"),
    message,
    fixed = TRUE
  )
})

test_that("check_slurm_partitions warns to increase ncpu when no smaller partition exists", {
  message <- paste(
    "number of requested CPUs (15) less than 50% of available CPUs in cpu32mem128gb (32)",
    "Consider increasing `ncpu`",
    sep = "\n"
  )

  expect_warning(
    check_slurm_partitions(15, "cpu32mem128gb"),
    message,
    fixed = TRUE
  )
})

test_that("toggle_logger messages what log level you've set", {
  initial_message <- "logging now at WARN level"
  expect_message(toggle_logger(), initial_message, fixed = TRUE)
  Sys.setenv("SLURMTOOLS_VERBOSE" = "INFO")
  new_message <- "logging now at INFO level"
  expect_message(toggle_logger(), new_message, fixed = TRUE)
})
