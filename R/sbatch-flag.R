# --- SbatchFlag: one planned `#SBATCH` line -------------------------------------
#
# A Template remembers each header line as what it is made of — the sbatch
# flag, the placeholder it is filled from, the value if one has been given, and
# whether the line is optional — not as text. Nothing is substituted until the
# template is built: format(), print(), write_slurm_template() and
# submit_slurm_job() all render from this record, so a value given to the verb
# and one given later with fill() produce the same line.

validate_sbatch_flag <- function(self) {
  if (length(self@flag) != 1 || is.na(self@flag) || !grepl("^[a-z][a-z0-9-]*$", self@flag)) {
    return("@flag must be an sbatch long option without the leading dashes, like `cpus-per-task`")
  }
  if (length(self@fill) != 1 || is.na(self@fill) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", self@fill)) {
    return("@fill must start with a letter and contain only letters, digits and `_`, like `ncpu`")
  }
  if (!is.null(self@value) && (!is.atomic(self@value) || length(self@value) != 1 || is.na(self@value))) {
    return("@value must be a single non-missing value (a string or a number), or NULL until filled")
  }
  if (length(self@optional) != 1 || is.na(self@optional)) {
    return("@optional must be TRUE or FALSE")
  }
  check <- sbatch_flags[[self@flag]]
  if (!is.null(self@value) && !is.null(check)) {
    problem <- check(self@value)
    if (!is.null(problem)) {
      return(sprintf(
        "`{{%s}}` fills `--%s`, which %s; got %s",
        self@fill,
        self@flag,
        problem,
        deparse(self@value)
      ))
    }
  }
  NULL
}

SbatchFlag <- S7::new_class(
  "SbatchFlag",
  package = "slurmtools",
  properties = list(
    flag = S7::class_character,
    fill = S7::class_character,
    value = S7::new_property(S7::class_any, default = NULL),
    optional = S7::new_property(S7::class_logical, default = FALSE)
  ),
  validator = validate_sbatch_flag
)

# the `sbatch` property: SbatchFlag objects in header order, each flag once
validate_sbatch_flags <- function(value) {
  ok <- vapply(value, function(x) S7::S7_inherits(x, SbatchFlag), logical(1))
  if (!all(ok)) {
    return("must be built with with_sbatch_flag() or the with_<flag>() verbs")
  }
  flags <- vapply(value, function(x) x@flag, character(1))
  dup <- unique(flags[duplicated(flags)])
  if (length(dup) > 0) {
    return(sprintf(
      "each flag may appear once; duplicated: %s",
      paste0("`", dup, "`", collapse = ", ")
    ))
  }
  NULL
}

# the header's flags, in order
sbatch_flag_names <- function(template) {
  vapply(template@sbatch, function(x) x@flag, character(1))
}

# the SbatchFlag behind `flag`, or NULL when the header has no such line
sbatch_entry <- function(template, flag) {
  i <- match(flag, sbatch_flag_names(template))
  if (is.na(i)) NULL else template@sbatch[[i]]
}

# the flags filled from `placeholder`
flags_filled_from <- function(template, placeholder) {
  fills <- vapply(template@sbatch, function(x) x@fill, character(1))
  sbatch_flag_names(template)[fills == placeholder]
}

# the flags emitted only when filled
optional_flags <- function(template) {
  opt <- vapply(template@sbatch, function(x) x@optional, logical(1))
  sbatch_flag_names(template)[opt]
}

# everything filled so far, by placeholder: the header's values, then the
# script's own fills. Flags filled from the same placeholder share its value.
template_fills <- function(template) {
  values <- list()
  for (entry in template@sbatch) {
    if (!is.null(entry@value)) {
      values[[entry@fill]] <- entry@value
    }
  }
  c(values, template@fills)
}

# the template without `flag`; a flag it never had is nothing to remove
without_flag <- function(template, flag) {
  template@sbatch <- template@sbatch[sbatch_flag_names(template) != flag]
  template
}

# `value` on every flag filled from `placeholder`
set_flag_value <- function(template, placeholder, value) {
  sbatch <- template@sbatch
  for (i in seq_along(sbatch)) {
    if (identical(sbatch[[i]]@fill, placeholder)) {
      entry <- sbatch[[i]]
      entry@value <- value
      sbatch[[i]] <- entry
    }
  }
  template@sbatch <- sbatch
  template
}
