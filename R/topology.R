# `sinfo` reports CPUs and memory *per node*, so every check in this file asks
# "does one node of this partition hold what the template asked for?" — the
# partition's totals never enter into it.

# the multiple of a megabyte each `--mem` suffix stands for
mem_unit_mb <- c(K = 1 / 1024, M = 1, G = 1024, T = 1024^2)

#' parse an sbatch `--mem` value into megabytes
#'
#' `--mem` takes an optional `[K|M|G|T]` suffix and defaults to megabytes, which
#' is the unit `sinfo`'s `%m` reports. Anything this does not recognise comes back
#' as `NA` so the caller leaves the decision to sbatch rather than guessing.
#'
#' @param value the filled `--mem` value
#' @return the request in MB, or `NA_real_`
#' @keywords internal
#' @noRd
parse_mem_mb <- function(value) {
  text <- toupper(trimws(as.character(value)))
  parts <- regmatches(text, regexec("^([0-9]+(\\.[0-9]+)?)([KMGT]?)$", text))[[1]]
  if (length(parts) == 0) {
    return(NA_real_)
  }
  unit <- if (nzchar(parts[[4]])) parts[[4]] else "M"
  as.numeric(parts[[2]]) * mem_unit_mb[[unit]]
}

#' the smallest partitions whose per-node memory covers the request
#'
#' @param mem_mb the request in MB
#' @param table the partition table
#' @return a one-line suggestion, in `partition_advice()`'s phrasing
#' @keywords internal
#' @noRd
memory_advice <- function(mem_mb, table) {
  fits <- table[table$MEMORY >= mem_mb, ]
  if (nrow(fits) == 0) {
    return(glue::glue(
      "No existing partition has {round(mem_mb)} MB per node; the largest is {max(table$MEMORY)} MB."
    ))
  }
  fits <- fits[order(fits$MEMORY, fits$CPUS), ]
  glue::glue("You might try {toString(utils::head(fits$PARTITION, 2))}")
}

#' check a filled template's resources against the partition's per-node limits
#'
#' Runs after [check_slurm_partitions()], which covers the cpus-per-task fit.
#' This adds the two limits it does not: `--mem` against the node's memory, and
#' `--gres` against a partition that advertises none. It also warns when a
#' multi-node request leaves cores idle — slurm allocates whole nodes, so
#' `--nodes 2 --cpus-per-task 2` on a 4-cpu partition reserves 8 CPUs and uses 2
#' (corpus ex. 2's rule is one whole node per task).
#'
#' A template without `--partition` is not checked, the same as
#' [check_slurm_partitions()].
#'
#' @param template the filled `Template`
#' @param partition name of the (already-validated) partition
#' @param cache optional argument to forgo caching
#' @return `partition`, invisibly
#' @keywords internal
#' @noRd
check_slurm_topology <- function(template, partition, cache = TRUE) {
  table <- cached_partition_table(cache)
  node <- table[table$PARTITION == partition, ]

  mem <- filled_flag(template, "mem")
  if (!is.null(mem)) {
    mem_mb <- parse_mem_mb(mem)
    # `--mem=0` asks for the node's whole memory, which always fits
    if (!is.na(mem_mb) && mem_mb > 0 && mem_mb > node$MEMORY) {
      rlang::abort(c(
        glue::glue(
          "requested memory (`--mem={mem}`) exceeds {partition}'s {node$MEMORY} MB per node"
        ),
        i = memory_advice(mem_mb, table)
      ))
    }
  }

  gres <- filled_flag(template, "gres")
  if (!is.null(gres) && is.na(node$GRES)) {
    with_gres <- table$PARTITION[!is.na(table$GRES)]
    rlang::abort(c(
      glue::glue("`--gres={gres}` requested but {partition} advertises no gres"),
      i = if (length(with_gres) > 0) {
        glue::glue("partitions with gres: {toString(with_gres)}")
      } else {
        "No partition on this cluster advertises gres."
      }
    ))
  }

  nodes <- filled_flag(template, "nodes")
  ncpu <- filled_flag(template, "cpus-per-task")
  if (!is.null(nodes) && !is.null(ncpu)) {
    nodes <- suppressWarnings(as.numeric(nodes))
    ncpu <- suppressWarnings(as.numeric(ncpu))
    if (!is.na(nodes) && !is.na(ncpu) && nodes > 1 && ncpu != node$CPUS) {
      rlang::warn(glue::glue(
        "distributed job: `--cpus-per-task={ncpu}` does not fill {partition}'s {node$CPUS} CPUs per node\n",
        "slurm allocates whole nodes, so {nodes} nodes reserve {nodes * node$CPUS} CPUs. ",
        "One whole node per task is the usual rule."
      ))
    }
  }

  invisible(partition)
}
