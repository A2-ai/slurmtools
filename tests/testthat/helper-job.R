# fixtures shared by the job lifecycle tests

# what submit_slurm_job() returns for a Template: sbatch's processx result plus
# the id and the paths, as a plain list
a_job <- function(id = "2051", output = sprintf("submission-log/sim-%s.out", id),
                  error = sprintf("submission-log/sim-%s.err", id)) {
  list(
    status = 0L,
    stdout = paste0(id, "\n"),
    stderr = "",
    timeout = FALSE,
    job_id = id,
    job_name = "sim",
    partition = "cpu2mem4gb",
    script = "submission-log/sim.sh",
    output = output,
    error = error
  )
}

# one sacct --allocations --parsable2 row for job `id` in `state`
sacct_row <- function(state, id = "2051", exit_code = "0:0") {
  sprintf(
    "%s|sim|cpu2mem4gb|%s|%s|00:00:11|cpu2mem4gb-dy-c7i-large-1|2026-09-15T19:53:47|2026-09-15T19:58:07|2026-09-15T19:58:18",
    id, state, exit_code
  )
}

# a stub sacct on the PATH that answers its k-th call with says[k] (the last
# answer repeats once they run out) and counts its calls in the returned file
stub_sacct_sequence <- function(says, env = parent.frame()) {
  stub_dir <- withr::local_tempdir(.local_envir = env)
  answers <- file.path(stub_dir, "answers")
  calls <- file.path(stub_dir, "calls")
  writeLines(says, answers)
  writeLines("0", calls)
  brio::write_file(
    sprintf(
      "#!/bin/bash\nn=$(( $(cat '%s') + 1 ))\necho \"$n\" > '%s'\nlast=$(wc -l < '%s')\n[ \"$n\" -gt \"$last\" ] && n=$last\nsed -n \"${n}p\" '%s'\n",
      calls, calls, answers, answers
    ),
    file.path(stub_dir, "sacct")
  )
  fs::file_chmod(file.path(stub_dir, "sacct"), "0755")
  withr::local_path(stub_dir, action = "prefix", .local_envir = env)
  calls
}

sacct_calls <- function(calls_file) as.integer(readLines(calls_file))
