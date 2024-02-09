
partition_cache <- new.env(parent = emptyenv())

#' get list of each partition's number of CPUs and memory
#'
#' @return the raw partition-cpu-memory string output from `sinfo`
run_sinfo <- function() {
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }

  res <- processx::run(sinfobin, c("--format", "%P,%c,%m"), )
  if (res$status != 0) {
    stderrout <- sprintf("failed to get partition info with info:
                         \n\n stdout: %s\n\n stderr: %s", res$stdout, res$stderr)
    rlang::abort(stderrout)
  }
  return(res$stdout)
}


#' get table of each partition's number of CPUs and memory
#'
#' * gets the raw string output from [run_sinfo()]
#' * converts to a data frame
#' * reorders and removes `*` from default partition with
#'    [process_slurm_partitions()]
#'
#' @param cache optional argument to forgo caching
#'
#' @return the processed table of each partition's
#'    number of CPUs and memory
lookup_partitions_by_cpu <- function(cache = TRUE) {
  avail_cpus <- if(cache) {
    if (is.null(partition_cache[["run_sinfo"]])) {
      partition_cache[["run_sinfo"]] <- run_sinfo()
    }
    partition_cache[["run_sinfo"]]
  } else {
    run_sinfo()
  }

  # make data frame
  avail_cpus_table <- read.table(text = avail_cpus, sep = ",", header = T)

  # process
  process_slurm_partitions(avail_cpus_table)
} # lookup_partitions_by_cpu



#' manipulate partition table for usability
#'
#' @param table
#'
#' @return the table such that the default partition will be first
#'    and will have the asterisk removed
process_slurm_partitions <- function(table){
  all_partitions <- table$PARTITION
  is_default_partition <- grepl('\\*$', x = all_partitions, )
  default_partition <- gsub(x = all_partitions[is_default_partition],
                            pattern = "\\*$", replacement = "")
  # delete the default partition then lets put it at the beginning
  # so it can be selected that way
  all_partitions <- all_partitions[-is_default_partition]
  table$PARTITION <- c(default_partition, all_partitions)
  return(table)
}


#' get list of partition names for the given cluster
#'
#' @param cache optional argument to forgo caching
#'
#' @export
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
#' In a call to [submit_slurm_model()], if the number of requested CPUs exceeds
#' the number of CPUs available in the requested partition,
#' [check_slurm_partitions()] errors. <br />
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
#'
#' @return string with suggestion upon [check_slurm_partitions()] error
partition_advice <- function(ncpu, partition, avail_cpus_table, cache) {
  library(dplyr)

  sorted_table <- avail_cpus_table %>% dplyr::filter(CPUS >= ncpu) %>%  dplyr::arrange(CPUS, MEMORY)
  list_of_partitions <- sorted_table$PARTITION
  if(length(list_of_partitions) > 1) {
    return(glue::glue("You might try {list_of_partitions[1]} or {list_of_partitions[2]}"))
  } else if (length(list_of_partitions) == 1) {
    return(glue::glue("You might try {list_of_partitions[1]}"))
  } else {
    return(glue::glue("Input a smaller value for ncpu. No existing partition has {ncpu} or more CPUs per node."))
  }

}


#' throws error if the number of requested CPUs exceeds
#' the number of CPUs available in the requested partition
#'
#' @param ncpu number of CPUs requested by user
#' @param partition name of partition requested by user
#' @param cache optional argument to forgo caching
#'
#' @export
#'
#' @examples
#' check_slurm_partitions(17, "cpu2mem4gb")
#' check_slurm_partitions(3, "cpu2mem4gb")
#' check_slurm_partitions(5, "cpu4mem32gb")
#' check_slurm_partitions(5, "cpu4mem32gb")
#' check_slurm_partitions(100, "cpu32mem128gb")
#' check_slurm_partitions(2, "cpu2mem4gb")
check_slurm_partitions <- function(ncpu, partition, cache = TRUE) {
  # if get_slurm_partitions has already run (which it definitely has),
  # the table will be cached
  avail_cpus_table <- if (cache) {
    if (is.null(partition_cache[["partition_by_cpu"]])) {
      partition_cache[["partition_by_cpu"]] <- lookup_partitions_by_cpu(cache)
    }
    partition_cache[["partition_by_cpu"]]
  } else {
    lookup_partitions_by_cpu()
  }

  # look up # of cpus in partition from table
  num_avail_cpus <- avail_cpus_table[avail_cpus_table$PARTITION == partition,]$CPUS

  # if # requested cpus > available CPUs, throw an error
  if(ncpu > num_avail_cpus) {
    suggestion <- partition_advice(ncpu, partition, avail_cpus_table, cache)
    rlang::abort(glue::glue("number of requested CPUs ({ncpu}) greater than number of available CPUs in {partition} ({num_avail_cpus})\n{suggestion}"))
  }
}

