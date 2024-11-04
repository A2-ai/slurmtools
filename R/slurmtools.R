#' slurmtools: An R package for easily submitting NONMEM jobs to slurm.
#'
#' This package aims to ease the submission and monitoring of NONMEM jobs running
#' on slurm.
#'
#' @section submitting jobs:
#' \itemize{
#'  \item \code{\link{submit_nonmem_model}}: Submits a job to slurm
#'  \item \code{\link{generate_nmm_config}}: Generates a NONMEMmonitor config
#'  file
#' }
#'
#' @section monitoring jobs:
#' \itemize{
#'  \item \code{\link{get_slurm_jobs}}: Gives a table of submitted jobs to slurm
#'  that shows status and other information given with `squeue`
#' }
#'
#' @section slurm partitions:
#' \itemize{
#'  \item \code{\link{get_slurm_partitions}}: Gives a vector of available
#'  partitions a user can submit jobs to.
#' }
#'
#' @name slurmtools
"_PACKAGE"


## usethis namespace: start
#' @importFrom brio read_file
#' @importFrom brio write_file
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom fs dir_create
#' @importFrom fs dir_delete
#' @importFrom fs dir_exists
#' @importFrom fs file_chmod
#' @importFrom fs file_exists
#' @importFrom fs is_absolute_path
#' @importFrom fs path_abs
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom jsonlite fromJSON
#' @importFrom processx run
#' @importFrom purrr discard
#' @importFrom purrr flatten_chr
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom purrr map_if
#' @importFrom purrr map2
#' @importFrom purrr map2_chr
#' @importFrom rlang abort
#' @importFrom rlang as_function
#' @importFrom rlang dots_list
#' @importFrom rlang is_atomic
#' @importFrom rlang is_missing
#' @importFrom rlang names2
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tibble tibble
#' @importFrom utils globalVariables
#' @importFrom utils read.table
#' @importFrom whisker whisker.render
#' @importFrom withr with_dir
## usethis namespace: end
NULL
