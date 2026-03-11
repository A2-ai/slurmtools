#' slurmtools: An R package for easily submitting NONMEM jobs to slurm.
#'
#' This package aims to ease the submission and monitoring of NONMEM jobs running
#' on slurm.
#'
#' @section submitting jobs:
#' \itemize{
#'  \item \code{\link{submit_slurm_job}}: Submits a job to slurm
#' }
#'
#' @section monitoring jobs:
#' \itemize{
#'  \item \code{\link{get_slurm_jobs}}: Gives a table of submitted jobs to slurm
#'  that shows status and other information given with `squeue`
#' }
#'
#' @section cancelling jobs:
#' \itemize{
#'  \item \code{\link{cancel_slurm_job}}: Cancels the specified job
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
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom utils globalVariables
#' @importFrom utils read.table
#' @importFrom whisker whisker.render
#' @importFrom withr with_dir
## usethis namespace: end
NULL
