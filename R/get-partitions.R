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

check_slurm_partitions <- function(ncpus) {
  sinfobin <- Sys.which("sinfo")
  if (!nzchar(sinfobin)) {
    rlang::abort("could not find sinfo binary")
  }

  avail_cpus <- processx::run(sinfobin, c("--format", "%P,%c"), )
  avail_cpus_text <- as.character(avail_cpus)
  avail_cpus_table <- read.delim(text = avail_cpus_text, sep = ",", skip = 1, blank.lines.skip = TRUE, col.names = c("PARTITIONS", "CPUS"))
  print(avail_cpus_table)

  sum_avail_cpus <- sum(avail_cpus_table$CPUS, na.rm = TRUE)

  if(ncpus > sum_avail_cpus) {
    rlang::abort("number of requested CPUs greater than number of available CPUs")
  }
}

