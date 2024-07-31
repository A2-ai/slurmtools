#' Creates a default watcher configuration file
#'
#' @param .mod a bbi nonmem model object
#' @param watch_opts an optional list to overwrite default values
#'
#' @return none
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#'   generate_watcher_config(.mod)
#' }
generate_watcher_config <- function(.mod, watch_opts = list()) {
  default_toml <- list(
    model_number = basename(.mod$absolute_model_path),
    files_to_track = c("lst", "ext", "grd"),
    tmp_dir = "/tmp",
    watched_dir = file.path("model", "nonmem"),
    output_dir = file.path("model", "nonmem", "in_progress"),
    poll_duration = 1,
    record_output = TRUE
  )

  toml <- utils::modifyList(default_toml, watch_opts)
  toml <- validate_toml(toml)

  config_toml_path <- paste0(.mod$absolute_model_path, ".toml")
  write_file(rextendr::to_toml(toml), config_toml_path)
}


#' Title
#'
#' @param toml_list a list representing the nmm config.toml file
#'
#' @return a potentially trimmed down list containing only actual config values
#' @export
#'
#' @examples \dontrun{
#'   validate_toml(list(model_number = 1001))
#' }
validate_toml <- function(toml_list) {
  config_keys = c(
    "model_number",
    "files_to_track",
    "tmp_dir",
    "watched_dir",
    "output_dir",
    "poll_duration",
    "record_output"
  )

  toml <- toml_list[intersect(names(toml_list), config_keys)]
  if (length(toml) == length(config_keys)) {
    return (toml)
  } else {
    print("Missing config parameters")
  }
}
