# this will take the raw string and return the partitions such that the default one will be first
# and will have the asterisk removed

partition_cache <- new.env(parent = emptyenv())

run_sinfo <- function(sinfobin) {
  # returns the whole partition/cpu table
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }

  res <- processx::run(sinfobin, c("--format", "%P,%c"), )
  if (res$status != 0) {
    stderrout <- sprintf("failed to get partition info with info: \n\n stdout: %s\n\n stderr: %s", res$stdout, res$stderr)
    rlang::abort(stderrout)
  }
  return(res$stdout)
}

lookup_partitions_by_cpu <- function(cache = TRUE) {
  # shell out for partitions
  # avail_cpus <- processx::run(sinfobin, c("--format", "%P,%c"), )
  avail_cpus <- if(cache) {
    if (is.null(partition_cache[["run_sinfo"]])) {
      partition_cache[["run_sinfo"]] <- run_sinfo(sinfobin)
    }
    partition_cache[["run_sinfo"]]
  } else {
    run_sinfo(sinfobin)
  }


  # make shell output character


  # take out * char for default node
  #avail_cpus_text <- gsub('[*]', '', avail_cpus_text)

  # make data frame
  avail_cpus_table <- read.table(text = avail_cpus, sep = ",", header = T)
  process_slurm_partitions(avail_cpus_table)
} # lookup_partitions_by_cpu


# process_slurm_partitions <- function(ps){
#   partitions <- strsplit(ps, "\n")[[1]]
#   all_partitions <- gsub("'", "", partitions)
#   is_default_partition <- grepl('\\*$', x = all_partitions, )
#   default_partition <- gsub(x = all_partitions[is_default_partition], pattern = "\\*$", replacement = "")
#   # delete the default partition then lets put it at the beginning so it can be selected that way
#   all_partitions <- all_partitions[-is_default_partition]
#   c(default_partition, all_partitions)
# }

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
  # sinfobin <- Sys.which("sinfo")
  # if (!nzchar(sinfobin)) {
  #   rlang::abort("could not find sinfo binary")
  # }
  res <- if(cache) {
    if (is.null(partition_cache[["run_sinfo"]])) {
      partition_cache[["run_sinfo"]] <- run_sinfo()
    }
    partition_cache[["run_sinfo"]]
  } else {
    run_sinfo()
  }

  # res <- processx::run(sinfobin, c("--format='%P'", "--noheader"), )
  # if (res$status != 0) {
  #   stderrout <- sprintf("failed to get partition info with info: \n\n stdout: %s\n\n stderr: %s", res$stdout, res$stderr)
  #   rlang::abort(stderrout)
  # }
  table <- lookup_partitions_by_cpu()

  # this will be a line separated list of partitions with an extra * for the default partition
  # for example it'll look something like
  # "'cpu2mem4gb*'\n'cpu4mem32gb'\n'cpu8mem64gb'\n'cpu2mem8gb'\n'cpu4mem16gb'\n'cpu16mem128gb'\n'cpu8mem32gb'\n'cpu16mem64gb'\n'cpu32mem128gb'\n"

  return(table$PARTITION)
}




check_slurm_partitions <- function(ncpus, partition, cache = TRUE) {

  avail_cpus_table <- if (cache) {
    if (is.null(partition_cache[["partition_by_cpu"]])) {
      partition_cache[["partition_by_cpu"]] <- lookup_partitions_by_cpu()
    }
    partition_cache[["partition_by_cpu"]]
  } else {
    lookup_partitions_by_cpu()
  }

  # look up # of cpus in partition from dataframe
  num_avail_cpus <- avail_cpus_table[avail_cpus_table$PARTITIONS == partition,]$CPUS

  # if # requested cpus > available CPUs, throw an error
  if(ncpus > num_avail_cpus) {
    rlang::abort(paste0("number of requested CPUs (", ncpus, ") greater than number of available CPUs (", num_avail_cpus, ")"))
  }
}

