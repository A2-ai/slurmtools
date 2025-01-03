#' Creates a default watcher configuration file
#'
#' @param .mod a bbi nonmem model object
#' @param model_number a string of model number e.g. 101a15. Default is pulled from .mod
#' @param files_to_track a vector of file extensions. If left blank, nmm will use .ext, .lst. and .grd.
#' @param tmp_dir a temporary directory location to run nonmem. If left blank nmm will use /tmp
#' @param watched_dir the directory where nonmem will output. Default is here::here()/model/nonmem
#' @param output_dir the directory where watcher will place log and any output files default is here::here()/model/nonmem/in_progress
#' @param bbi_config_path the path to bbi.yaml file
#' @param poll_duration the amount of time in seconds between the watchers polling. If left blank, nmm will use 1 second
#' @param level What level to log at. Available options are Trace, Debug, Info, Warn, Fatal if left blank, nmm will use Info
#' @param threads number of threads if running a parallel job. If left blank nmm will use 1
#' @param overwrite bool whether to overwrite existing run.
#' @param alerter_opts a list for setting up an alerter fields needed are alerter, command, message_flag, use_stdout, args (list of additional args to be passed to alerter)
#' @return none
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#'   generate_nmm_config(.mod)
#' }
generate_nmm_config <- function(
    .mod,
    model_number = NULL,
    files_to_track = NULL,
    tmp_dir = NULL,
    watched_dir = file.path("model", "nonmem"),
    output_dir = file.path(watched_dir, "in_progress"),
    poll_duration = NULL,
    level = NULL,
    threads = NULL,
    overwrite = FALSE,
    bbi_config_path = NULL,
    alerter_opts = list()
) {
  if (is.null(model_number)) {
    model_number <- basename(.mod$absolute_model_path)
  }

  if (!is.null(level)) {
    level <- paste0(toupper(substring(level, 1, 1)), substring(level, 2))
  }

  if (watched_dir == file.path("model", "nonmem")) {
    watched_dir <- file.path(here::here(), watched_dir)
  }
  if (output_dir == file.path("model", "nonmem", "in_progress")) {
    output_dir <- file.path(watched_dir, "in_progress")
  }

  if (is.null(bbi_config_path)) {
    bbi_option_path <- getOption("slurmtools.bbi_config_path")
    if (!is.null(bbi_option_path)) {
      bbi_config_path <- bbi_option_path
    } else {
      warning("bbi.yaml path not supplied through options('slurmtools.bbi_config_path') or through arguments")
    }
  }

  toml <- list(
    model_number = model_number,
    files_to_track = files_to_track,
    tmp_dir = tmp_dir,
    watched_dir = watched_dir,
    output_dir = output_dir,
    bbi_config_path = bbi_config_path,
    poll_duration = poll_duration,
    level = level,
    threads = threads,
    overwrite = overwrite
  )

  toml <- purrr::compact(toml)
  config_toml_path <- paste0(.mod$absolute_model_path, ".toml")
  alerter_opts <- purrr::compact(alerter_opts)

  if (length(alerter_opts) != 0) {
    if ("args" %in% names(alerter_opts)) {
      write_file(to_toml(
        toml,
        alerter = alerter_opts[names(alerter_opts) != "args"],
        alerter.args = alerter_opts$args),
        config_toml_path)
    } else {
      write_file(to_toml(toml, alerter = alerter_opts), config_toml_path)
    }
  } else {
    write_file(to_toml(toml), config_toml_path)
  }
}
