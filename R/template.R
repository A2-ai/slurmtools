validate_sbatch_flags <- function(value) {
  flags <- names(value)
  if (length(value) > 0 && (is.null(flags) || anyNA(flags) || !all(nzchar(flags)))) {
    return(
      "must be a named character vector: names are sbatch flags, values are placeholder names"
    )
  }
  bad_flag <- flags[!grepl("^[a-z][a-z0-9-]*$", flags)]
  if (length(bad_flag) > 0) {
    return(sprintf(
      "flags are sbatch long options without the leading dashes, like `cpus-per-task`; not %s",
      paste0("`", bad_flag, "`", collapse = ", ")
    ))
  }
  dup <- unique(flags[duplicated(flags)])
  if (length(dup) > 0) {
    return(sprintf(
      "each flag may appear once; duplicated: %s",
      paste0("`", dup, "`", collapse = ", ")
    ))
  }
  bad_fill <- value[!grepl("^[A-Za-z][A-Za-z0-9_]*$", value)]
  if (length(bad_fill) > 0) {
    return(sprintf(
      "placeholder names must start with a letter and contain only letters, digits and `_`, like `ncpu`; not %s",
      paste0("`", bad_fill, "`", collapse = ", ")
    ))
  }
  NULL
}

#' A slurm job template, built one piece at a time
#'
#' @description
#' `Template()` starts an empty job template. Each `with_*()` verb adds one
#' `#SBATCH` line whose value is a `{{placeholder}}`, filled later at submit
#' time. `format()` returns the rendered script lines and `print()` shows
#' them.
#'
#' Flags are held as a named character vector: names are the sbatch long
#' options (`partition`, `cpus-per-task`), values are the placeholder each is
#' filled from. The object validates itself whenever it changes: a flag with
#' leading dashes, a duplicated flag, or a placeholder that could not be an R
#' argument name is refused on the spot.
#'
#' @param sbatch named character vector of `#SBATCH` flags in header order,
#'   e.g. `c(partition = "partition", "cpus-per-task" = "ncpu")`. Usually built
#'   with [with_sbatch()] rather than passed directly.
#'
#' @examples
#' Template() |>
#'   with_sbatch("job-name") |>
#'   with_sbatch("cpus-per-task", "ncpu") |>
#'   with_sbatch("partition")
#' @export
Template <- S7::new_class(
  "Template",
  package = "slurmtools",
  properties = list(
    sbatch = S7::new_property(
      S7::class_character,
      default = character(),
      validator = validate_sbatch_flags
    )
  )
)

#' Add an `#SBATCH` flag to a template
#'
#' @description
#' The blanket verb: any sbatch long option becomes the header line
#' `#SBATCH --<flag>={{<fill_name>}}`. Adding a flag the template already has
#' replaces its placeholder in place.
#'
#' @param template a [Template()]
#' @param flag an sbatch long option without the leading dashes, e.g.
#'   `"partition"`, `"cpus-per-task"`, `"gres"`
#' @param fill_name the placeholder the flag is filled from; defaults to the
#'   flag with `-` replaced by `_`, so `"cpus-per-task"` fills from
#'   `{{cpus_per_task}}`. Name it to let one value feed several places:
#'   `with_sbatch("cpus-per-task", "ncpu")`.
#'
#' @return the template with the flag added
#'
#' @examples
#' Template() |>
#'   with_sbatch("partition") |>
#'   with_sbatch("gres", "gpu")
#' @export
with_sbatch <- function(template, flag, fill_name = NULL) {
  if (!S7::S7_inherits(template, Template)) {
    rlang::abort(sprintf(
      "`template` must be a Template(), not a <%s>",
      class(template)[[1]]
    ))
  }
  if (!is.character(flag) || length(flag) != 1 || is.na(flag)) {
    rlang::abort("`flag` must be a single sbatch option name, like \"partition\"")
  }
  if (is.null(fill_name)) {
    fill_name <- gsub("-", "_", flag, fixed = TRUE)
  }
  if (!is.character(fill_name) || length(fill_name) != 1 || is.na(fill_name)) {
    rlang::abort("`fill_name` must be a single placeholder name, like \"ncpu\"")
  }

  sbatch <- template@sbatch
  sbatch[[flag]] <- fill_name
  template@sbatch <- sbatch
  template
}

#' @export
S7::method(format, Template) <- function(x, ...) {
  c(
    "#!/bin/bash",
    sprintf("#SBATCH --%s={{%s}}", names(x@sbatch), x@sbatch)
  )
}

#' @export
S7::method(print, Template) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
