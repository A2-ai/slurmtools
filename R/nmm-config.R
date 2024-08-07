#' Creates a default watcher configuration file
#'
#' @param .mod a bbi nonmem model object
#' @param model_number a string of model number e.g. 101a15. Default is pulled from .mod
#' @param files_to_track a vector of file extensions. Default is c("lst", "ext", "grd")
#' @param tmp_dir a temporary directory location to run nonmem. Default is /tmp
#' @param watched_dir the directory where nonmem will output. Default is here::here()/model/nonmem
#' @param output_dir the directory where watcher will place log and any output files default is here::here()/model/nonmem/in_progress
#' @param poll_duration the amount of time in seconds between the watchers polling
#' @param alert Where to send alerts if any. Default is None, available options are None or Slack
#' @param level What level to log at. Default is info. Available options are Trace, Debug, Info, Warn, Fatal
#' @param email if alert is set to Slack, this should be the email associated with slack to get messages sent directly to you.
#' @param threads number of threads if running a parallel job. Default is 1
#'
#' @return none
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#'   generate_nmm_config(.mod)
#' }
generate_nmm_config <- function(
    .mod,
    model_number = "",
    files_to_track = c("lst", "ext", "grd"),
    tmp_dir = "/tmp",
    watched_dir = file.path("model", "nonmem"),
    output_dir = file.path(watched_dir, "in_progress"),
    poll_duration = 1,
    alert = "None",
    level = "Debug",
    email = "",
    threads = 1
) {
  if (model_number == "") {
    model_number <- basename(.mod$absolute_model_path)
  }

  alert <- paste0(toupper(substring(alert, 1, 1)), substring(alert, 2))
  level <- paste0(toupper(substring(level, 1, 1)), substring(level, 2))

  if (alert == "Slack" && email == "") {
    rlang::abort("If you want slack notifications you must also supply an email.")
  }

  if (watched_dir == file.path("model", "nonmem")) {
    watched_dir <- file.path(here::here(), watched_dir)
  }
  if (output_dir == file.path("model", "nonmem", "in_progress")) {
    output_dir <- file.path(watched_dir, "in_progress")
  }

  toml <- list(
    model_number = model_number,
    files_to_track = files_to_track,
    tmp_dir = tmp_dir,
    watched_dir = watched_dir,
    output_dir = output_dir,
    poll_duration = poll_duration,
    alert = alert,
    level = level,
    email = email,
    threads = threads
  )

  config_toml_path <- paste0(.mod$absolute_model_path, ".toml")
  write_file(rextendr::to_toml(toml), config_toml_path)
}
