
partition_cache <- new.env(parent = emptyenv())

run_sinfo <- function(sinfobin) {
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }

  res <- processx::run(sinfobin, c("--format", "%P,%c,%m"), )
  if (res$status != 0) {
    stderrout <- sprintf("failed to get partition info with info: \n\n stdout: %s\n\n stderr: %s", res$stdout, res$stderr)
    rlang::abort(stderrout)
  }
  return(res$stdout)
}


lookup_partitions_by_cpu <- function(cache = TRUE) {
  avail_cpus <- if(cache) {
    if (is.null(partition_cache[["run_sinfo"]])) {
      partition_cache[["run_sinfo"]] <- run_sinfo(sinfobin)
    }
    partition_cache[["run_sinfo"]]
  } else {
    run_sinfo(sinfobin)
  }

  # make data frame
  avail_cpus_table <- read.table(text = avail_cpus, sep = ",", header = T)

  # process
  process_slurm_partitions(avail_cpus_table)
} # lookup_partitions_by_cpu


# this will return the partitions such that the default one
# will be first and will have the asterisk removed
process_slurm_partitions <- function(table){
  all_partitions <- table$PARTITION
  is_default_partition <- grepl('\\*$', x = all_partitions, )
  default_partition <- gsub(x = all_partitions[is_default_partition], pattern = "\\*$", replacement = "")
  # delete the default partition then lets put it at the beginning so it can be selected that way
  all_partitions <- all_partitions[-is_default_partition]
  table$PARTITION <- c(default_partition, all_partitions)
  return(table)
}


#' get slurm partitions for the given cluster
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

partition_advice <- function(ncpus, partition, cache) {
  library(dplyr)
  library(glue)
  table <- if (cache) {
    if (is.null(partition_cache[["partition_by_cpu"]])) {
      partition_cache[["partition_by_cpu"]] <- lookup_partitions_by_cpu(cache)
    }
    partition_cache[["partition_by_cpu"]]
  } else {
    lookup_partitions_by_cpu(cache)
  }

  # take out partition (not necessary, but could help later)
  # table <- table[table$PARTITION != partition, ]

  table2 <- table %>% filter(CPUS >= ncpus) %>%  arrange(CPUS, MEMORY)
  list_of_partitions <- table2$PARTITION
  if(length(list_of_partitions) > 1) {
    return(glue("You might try {list_of_partitions[1]} or {list_of_partitions[2]}"))
  } else if (length(list_of_partitions) == 1) {
    return(glue("You might try {list_of_partitions[1]}"))
  } else {
    return(glue("Input a smaller value for ncpu. No existing partition has {ncpus} or more CPUs per node."))
  }

}


check_slurm_partitions <- function(ncpus, partition, cache = TRUE) {
  library(glue)
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
  if(ncpus > num_avail_cpus) {
    suggestion <- partition_advice(ncpus, partition, cache)
    rlang::abort(glue("number of requested CPUs ({ncpus}) greater than number of available CPUs in {partition} ({num_avail_cpus})\n{suggestion}"))
  }
}

