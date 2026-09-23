# A partition table shaped like the one cached_partition_table() returns, with a
# gres partition devcluster does not have — PLAN § 6.4 (E9) notes the gres branch
# cannot be exercised for real here, so it is exercised against this fixture.
a_partition_table <- function() {
  data.frame(
    PARTITION = c("cpu2mem4gb", "cpu4mem32gb", "cpu32mem128gb", "gpu1"),
    CPUS = c(2L, 4L, 32L, 8L),
    MEMORY = c(3891L, 31129L, 124518L, 62259L),
    GRES = c(NA, NA, NA, "gpu:tesla:1")
  )
}

# swap the cached partition table for `table` until the calling test ends, so the
# topology checks need neither a cluster nor a real sinfo
local_partition_table <- function(table = a_partition_table(), env = parent.frame()) {
  old <- partition_cache[["partition_by_cpu"]]
  partition_cache[["partition_by_cpu"]] <- table
  withr::defer(partition_cache[["partition_by_cpu"]] <- old, envir = env)
  invisible(table)
}
