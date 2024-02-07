# this will take the raw string and return the partitions such that the default one will be first
# and will have the asterisk removed
process_slurm_partitions <- function(ps){
  partitions <- strsplit(ps, "\n")[[1]]
  all_partitions <- gsub("'", "", partitions)
  is_default_partition <- grepl('\\*$', x = all_partitions, )
  default_partition <- gsub(x = all_partitions[is_default_partition], pattern = "\\*$", replacement = "")
  # delete the default partition then lets put it at the beginning so it can be selected that way
  all_partitions <- all_partitions[-is_default_partition]
  c(default_partition, all_partitions)
}

#' get slurm partitions for the given cluster
#' @export
get_slurm_partitions <- function() {
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }
  res <- processx::run(sinfobin, c("--format='%P'", "--noheader"), )
  if (res$status != 0) {
    stderrout <- sprintf("failed to get partition info with info: \n\n stdout: %s\n\n stderr: %s", res$stdout, res$stderr)
    rlang::abort(stderrout)
  }
  # this will be a line separated list of partitions with an extra * for the default partition
  # for example it'll look something like
  # "'cpu2mem4gb*'\n'cpu4mem32gb'\n'cpu8mem64gb'\n'cpu2mem8gb'\n'cpu4mem16gb'\n'cpu16mem128gb'\n'cpu8mem32gb'\n'cpu16mem64gb'\n'cpu32mem128gb'\n"
  process_slurm_partitions(res$stdout)
}

check_slurm_partitions <- function(ncpus, partition) {
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }
  # shell out for partitions
  avail_cpus <- processx::run(sinfobin, c("--format", "%P,%c"), )

  # make shell output character
  avail_cpus_text <- as.character(avail_cpus)

  # take out * char for default node
  avail_cpus_text <- gsub('[*]', '', avail_cpus_text)

  # make data frame
  avail_cpus_table <- read.delim(text = avail_cpus_text, sep = ",",
                                 skip = 1, col.names = c("PARTITIONS", "CPUS"))

  # look up # of cpus in partition from dataframe
  num_avail_cpus <- avail_cpus_table[avail_cpus_table$PARTITIONS == partition,]$CPUS

  # if # requested cpus > available CPUs, throw an error
  if(ncpus > num_avail_cpus) {
    rlang::abort(paste0("number of requested CPUs (", ncpus, ") greater than number of available CPUs (", num_avail_cpus, ")"))
  }
}

