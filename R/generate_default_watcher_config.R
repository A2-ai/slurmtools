#' Creates a default watcher configuration file
#'
#' @param .mod a bbi nonmem model object
#'
#' @return none
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' generate_default_watcher_config(.mod)
#' }
generate_default_watcher_config <- function(.mod) {
  toml <- list(
    model_number = basename(.mod$absolute_model_path),
    files_to_track = c("lst", "ext", "grd"),
    tmp_dir = "/tmp",
    watched_dir = file.path("model", "nonmem"),
    output_dir = file.path("model", "nonmem", "in_progress"),
    poll_duration = 1,
    record_output = TRUE
  )

  config_toml_path <- paste0(.mod$absolute_model_path, ".toml")
  write_file(rextendr::to_toml(toml), config_toml_path)
}
