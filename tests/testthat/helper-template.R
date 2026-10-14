# the first line format() writes: the bash Sys.which() finds on this host
shebang_line <- paste0("#!", unname(Sys.which("bash")))

# the header as flag -> placeholder, for assertions on which blanks a template has
header_fills <- function(tmpl) {
  stats::setNames(vapply(tmpl@sbatch, function(f) f@fill, character(1)), sbatch_flag_names(tmpl))
}
