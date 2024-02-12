test_that("get_slurm_partitions works", {
  vectorOfPartitions <- c("cpu2mem4gb", "cpu4mem32gb", "cpu8mem64gb",
                           "cpu2mem8gb", "cpu4mem16gb", "cpu16mem128gb",
                           "cpu8mem32gb", "cpu16mem64gb", "cpu32mem128gb")

  expect_equal(get_slurm_partitions(), vectorOfPartitions)
})

