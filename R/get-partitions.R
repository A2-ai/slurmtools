utils::globalVariables(c("CPUS", "MEMORY"))

partition_cache <- new.env(parent = emptyenv())

#' get list of each partition's CPUs, memory and gres
#'
#' @return the raw partition-cpu-memory-gres string output from `sinfo`
#' @keywords internal
#' @noRd
run_sinfo <- function() {
  run_slurm("sinfo", c("--format", "%P|%c|%m|%G"))$stdout
}


#' get table of each partition's number of CPUs and memory
#'
#' * gets the raw string output from `run_sinfo()`
#' * converts to a data frame
#' * reorders and removes `*` from default partition with
#'    `process_slurm_partitions()`
#'
#' @param cache optional argument to forgo caching
#' @return the processed table of each partition's
#'    number of CPUs and memory
#' @keywords internal
#' @noRd
lookup_partitions_by_cpu <- function(cache = TRUE) {
  avail_cpus <- if (cache) {
    if (is.null(partition_cache[["run_sinfo"]])) {
      partition_cache[["run_sinfo"]] <- run_sinfo()
    }
    partition_cache[["run_sinfo"]]
  } else {
    run_sinfo()
  }

  # make data frame: every column read as text, then CPUS and MEMORY turned
  # into numbers here, so a grouped value like `4+` (sinfo's "4 or more",
  # printed when a partition's nodes differ) cannot turn the column into text
  avail_cpus_table <- read.table(
    text = avail_cpus,
    sep = "|",
    header = TRUE,
    colClasses = "character",
    na.strings = c("NA", "(null)"),
    strip.white = TRUE
  )
  avail_cpus_table$CPUS <- sinfo_count(avail_cpus_table$CPUS)
  avail_cpus_table$MEMORY <- sinfo_count(avail_cpus_table$MEMORY)

  # process
  process_slurm_partitions(avail_cpus_table)
} # lookup_partitions_by_cpu

# an sinfo count such as `4` or `4+` as an integer; the `+` marks the smallest
# value across nodes that differ, so what is kept is a lower bound
sinfo_count <- function(x) {
  as.integer(sub("\\+$", "", x))
}


#' manipulate partition table for usability
#'
#' @param table the table created by lookup_partitions_by_cpu
#'
#' @return the table with one row per partition, the default partition (the
#'    one sinfo stars) first with its asterisk removed, and the rest in sinfo's
#'    order. A table with no starred row is returned in sinfo's order; every
#'    later check then treats the first partition as the default.
#' @keywords internal
#' @noRd
process_slurm_partitions <- function(table) {
  # whole rows move with their names: the old code reordered the PARTITION
  # column alone, so with the default anywhere but first every name sat beside
  # another partition's CPUs and memory (deep-review finding 1)
  is_default <- grepl("\\*$", table$PARTITION)
  table$PARTITION <- sub("\\*$", "", table$PARTITION)
  default_names <- unique(table$PARTITION[is_default])

  # sinfo prints one row per group of identically configured nodes, so a
  # partition mixing two node shapes (CPU and GPU nodes, say) has two rows;
  # keep one per partition so every check reads a single number
  table <- one_row_per_partition(table)

  is_default <- table$PARTITION %in% default_names
  table <- table[order(!is_default), , drop = FALSE] # stable: default first, rest as sinfo listed them
  rownames(table) <- NULL
  table
}

# one row per partition name, in first-appearance order, with the largest CPUs
# and memory among its rows and the first gres any of its rows advertises
one_row_per_partition <- function(table) {
  if (!anyDuplicated(table$PARTITION)) {
    return(table)
  }
  groups <- split(seq_len(nrow(table)), factor(table$PARTITION, levels = unique(table$PARTITION)))
  rows <- lapply(groups, function(i) {
    out <- table[i[[1]], , drop = FALSE]
    out$CPUS <- largest(table$CPUS[i])
    out$MEMORY <- largest(table$MEMORY[i])
    if ("GRES" %in% names(table)) {
      gres <- table$GRES[i]
      out$GRES <- if (all(is.na(gres))) gres[[1]] else gres[!is.na(gres)][[1]]
    }
    out
  })
  do.call(rbind, unname(rows))
}

largest <- function(x) {
  if (all(is.na(x))) x[[1]] else max(x, na.rm = TRUE)
}

#' Gets the available slurm partitions the user can use.
#'
#' @param cache optional argument to forgo caching
#'
#' @return vector of available partitions
#' @export
#'
#' @examples \dontrun{
#' get_slurm_partitions
#' }
get_slurm_partitions <- function(cache = TRUE) {
  table <- if (cache) {
    if (is.null(partition_cache[["partition_by_cpu"]])) {
      partition_cache[["partition_by_cpu"]] <- lookup_partitions_by_cpu(cache)
    }
    partition_cache[["partition_by_cpu"]]
  } else {
    lookup_partitions_by_cpu(cache)
  }

  return(table$PARTITION)
}

#' get partition suggestions
#'
#' In a call to `submit_slurm_model()`, if the number of requested CPUs exceeds
#' the number of CPUs available in the requested partition,
#' `check_slurm_partitions()` errors. <br />
#' This function follows up with a message providing one or two suggestions for
#' the partition with the smallest sufficient number of CPUs and least amount
#' of memory. <br />
#' If there are no partitions with enough CPUs to accommodate the number
#' requested, this function's return message clarifies this.
#'
#' @param ncpu number of CPUs requested by user
#' @param partition name of partition requested by user
#' @param avail_cpus_table table of partitions with respective number of CPUs and memory
#' @param cache optional argument to forgo caching
#' @param underutilized whether the advice is for an underutilization warning
#'
#' @return string with suggestion upon `check_slurm_partitions()` error or warning
#'
#' @keywords internal
#' @noRd
partition_advice <- function(
  ncpu,
  partition,
  avail_cpus_table,
  cache,
  underutilized = FALSE
) {
  sorted_table <- avail_cpus_table %>%
    dplyr::filter(CPUS >= ncpu) %>%
    dplyr::arrange(CPUS, MEMORY)
  list_of_partitions <- sorted_table$PARTITION

  if (underutilized) {
    suggested_partitions <- list_of_partitions[list_of_partitions != partition]
    if (length(suggested_partitions) > 1) {
      return(glue::glue(
        "Consider increasing `ncpu` or using a smaller partition\nYou might try {suggested_partitions[1]} or {suggested_partitions[2]}"
      ))
    } else if (length(suggested_partitions) == 1) {
      return(glue::glue(
        "Consider increasing `ncpu` or using a smaller partition\nYou might try {suggested_partitions[1]}"
      ))
    } else {
      return("Consider increasing `ncpu`")
    }
  }

  if (length(list_of_partitions) > 1) {
    return(glue::glue(
      "You might try {list_of_partitions[1]} or {list_of_partitions[2]}"
    ))
  } else if (length(list_of_partitions) == 1) {
    return(glue::glue("You might try {list_of_partitions[1]}"))
  } else {
    return(glue::glue(
      "Input a smaller value for ncpu. No existing partition has {ncpu} or more CPUs per node."
    ))
  }
}


#' load the (cached) partition / cpu / memory table
#'
#' Shared by [get_slurm_partitions()]-adjacent helpers so the cache-lookup lives
#' in one place.
#'
#' @param cache optional argument to forgo caching
#' @return the processed partition table
#' @keywords internal
#' @noRd
cached_partition_table <- function(cache = TRUE) {
  # if get_slurm_partitions has already run (which it usually has), the table
  # will be cached
  if (cache) {
    if (is.null(partition_cache[["partition_by_cpu"]])) {
      partition_cache[["partition_by_cpu"]] <- lookup_partitions_by_cpu(cache)
    }
    partition_cache[["partition_by_cpu"]]
  } else {
    lookup_partitions_by_cpu()
  }
}

#' resolve and validate the requested partition
#'
#' Resolves the `partition = get_slurm_partitions()` default (the whole vector of
#' partitions, of which the first / smallest is taken) and confirms the choice is
#' a real partition. This is the *validate* half; [check_slurm_partitions()] is
#' the *check* half that confirms a requested cpu count fits the resolved
#' partition.
#'
#' @param partition name of the partition, or the vector of available partitions
#' @param cache optional argument to forgo caching
#' @return the single, validated partition name
#' @keywords internal
#' @noRd
validate_partition <- function(partition, cache = TRUE) {
  avail_cpus_table <- cached_partition_table(cache)

  if (is.null(partition)) {
    rlang::abort("no partition selected")
  }
  if (length(partition) > 1) {
    partition <- partition[[1]]
  }
  if (!partition %in% avail_cpus_table$PARTITION) {
    rlang::abort(glue::glue(
      "`{partition}` is not an available partition.\nAvailable: {toString(avail_cpus_table$PARTITION)}"
    ))
  }

  partition
}

#' check the requested cpu count against the resolved partition
#'
#' Errors if the number of requested CPUs exceeds the partition's CPUs, and warns
#' if the request uses less than half of them (underutilization). Expects
#' `partition` to already be resolved and validated by [validate_partition()].
#'
#' @param ncpu number of CPUs requested by user
#' @param partition name of the (already-validated) partition
#' @param cache optional argument to forgo caching
#'
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' check_slurm_partitions(17, "cpu2mem4gb")
#' check_slurm_partitions(3, "cpu2mem4gb")
#' check_slurm_partitions(5, "cpu4mem32gb")
#' check_slurm_partitions(5, "cpu4mem32gb")
#' check_slurm_partitions(100, "cpu32mem128gb")
#' check_slurm_partitions(2, "cpu2mem4gb")
#' }
check_slurm_partitions <- function(ncpu, partition, cache = TRUE) {
  avail_cpus_table <- cached_partition_table(cache)

  # look up # of cpus in partition from table
  num_avail_cpus <- avail_cpus_table[
    avail_cpus_table$PARTITION == partition,
  ]$CPUS

  # if # requested cpus > available CPUs, throw an error
  if (ncpu > num_avail_cpus) {
    suggestion <- partition_advice(ncpu, partition, avail_cpus_table, cache)
    rlang::abort(glue::glue(
      "number of requested CPUs ({ncpu}) greater than number of available CPUs in {partition} ({num_avail_cpus})\n{suggestion}"
    ))
  } else if (ncpu < 0.5 * num_avail_cpus) {
    rlang::warn(glue::glue(
      "number of requested CPUs ({ncpu}) less than 50% of available CPUs in {partition} ({num_avail_cpus})\nAlternative submission strategies may result in more optimized resource usage."
    ))
  }

  invisible(partition)
}
