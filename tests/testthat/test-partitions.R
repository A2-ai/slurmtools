# process_slurm_partitions() and the sinfo read behind it, on tables shaped like
# clusters other than devcluster (deep-review finding 1). No sinfo needed.

parsed <- function(...) process_slurm_partitions(data.frame(...))

test_that("the starred default partition moves to the top as a whole row", {
  out <- parsed(PARTITION = c("a", "b*", "c"), CPUS = c(2, 4, 8), MEMORY = c(10, 20, 30))
  expect_equal(out$PARTITION, c("b", "a", "c"))
  expect_equal(out$CPUS, c(4, 2, 8))
  expect_equal(out$MEMORY, c(20, 10, 30))
})

test_that("a default already listed first is left alone, star removed", {
  out <- parsed(PARTITION = c("a*", "b", "c"), CPUS = c(2, 4, 8))
  expect_equal(out$PARTITION, c("a", "b", "c"))
  expect_equal(out$CPUS, c(2, 4, 8))
})

test_that("no starred partition: sinfo's order is kept and nothing is dropped", {
  out <- parsed(PARTITION = c("a", "b", "c"), CPUS = c(2, 4, 8))
  expect_equal(out$PARTITION, c("a", "b", "c"))
  expect_equal(out$CPUS, c(2, 4, 8))
})

test_that("a partition printed on two rows (two node shapes) becomes one row with the largest figures", {
  out <- parsed(
    PARTITION = c("a*", "a*", "b"),
    CPUS = c(2L, 4L, 8L),
    MEMORY = c(3891L, 31129L, 62259L),
    GRES = c(NA, "gpu:tesla:1", NA)
  )
  expect_equal(nrow(out), 2)
  expect_equal(out$PARTITION, c("a", "b"))
  expect_equal(out$CPUS, c(4L, 8L))
  expect_equal(out$MEMORY, c(31129L, 62259L))
  expect_equal(out$GRES, c("gpu:tesla:1", NA))
})

test_that("devcluster's own rows, with the default listed second, parse to the right figures", {
  # the 2026-11-02 cluster probe: cpu4mem32gb's figures landed beside the default's name
  out <- parsed(
    PARTITION = c("cpu4mem32gb", "cpu2mem4gb*", "cpu8mem64gb"),
    CPUS = c(4L, 2L, 8L),
    MEMORY = c(31129L, 3891L, 62259L)
  )
  expect_equal(out$PARTITION, c("cpu2mem4gb", "cpu4mem32gb", "cpu8mem64gb"))
  expect_equal(out$CPUS, c(2L, 4L, 8L))
  expect_equal(out$MEMORY, c(3891L, 31129L, 62259L))
})

test_that("a non-default partition on two rows collapses too, and the checks see one number", {
  table <- parsed(
    PARTITION = c("a*", "mixed", "mixed"),
    CPUS = c(2L, 4L, 16L),
    MEMORY = c(3891L, 31129L, 124518L),
    GRES = c(NA, NA, "gpu:a100:2")
  )
  local_partition_table(table)
  expect_no_error(check_slurm_partitions(16, "mixed"))
  expect_error(check_slurm_partitions(17, "mixed"), "greater than number of available CPUs in mixed (16)", fixed = TRUE)
})

test_that("sinfo text with grouped `4+` values and (null) gres reads as numbers and NA", {
  old <- partition_cache[["run_sinfo"]]
  partition_cache[["run_sinfo"]] <- paste(
    "PARTITION|CPUS|MEMORY|GRES",
    "cpu2mem4gb*|2|3891|(null)",
    "mixed|4+|31129+|(null)",
    "gpu1|8|62259|gpu:tesla:1",
    sep = "\n"
  )
  withr::defer(partition_cache[["run_sinfo"]] <- old)

  out <- lookup_partitions_by_cpu(cache = TRUE)
  expect_type(out$CPUS, "integer")
  expect_type(out$MEMORY, "integer")
  expect_equal(out$PARTITION, c("cpu2mem4gb", "mixed", "gpu1"))
  expect_equal(out$CPUS, c(2L, 4L, 8L))
  expect_equal(out$MEMORY, c(3891L, 31129L, 62259L))
  expect_equal(out$GRES, c(NA, NA, "gpu:tesla:1"))
})
