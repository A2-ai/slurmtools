
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/slurmtools/actions/workflows/RunChecks.yaml/badge.svg)](https://github.com/A2-ai/slurmtools/actions/workflows/RunChecks.yaml)
<!-- badges: end -->

`slurmtools` is a collection of utility functions suitable for
interacting with slurm and submitting nonmem jobs. There are three main
functions of `slurmtools`

1.  `submit_slurm_jobs` - submits a job to slurm via `sbatch`. The job
    is defined by a template file that is filled in during the call of
    this function
2.  `get_slurm_jobs` - gives a tibble of currently runing jobs via
    `squeue`.
3.  `cancel_slurm_jobs` - cancels a running slurm job via `scancel`.

## Installation

You can install the development version of slurmtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("a2-ai/slurmtools")
```

## Examples

This block sets up options used by slurmtools:

``` r
nonmem = file.path(here::here(), "vignettes", "model", "nonmem")

options('slurmtools.submission_root' = file.path(nonmem, "submission-log"))
options('slurmtools.bbi_config_path' = file.path(nonmem, "bbi.yaml"))
options('slurmtools.slurm_job_template_path' = file.path(nonmem, 'slurm-job-bbi.tmpl'))

library(slurmtools)
#> 
#> 
#> ── Set slurmtools options ──────────────────────────────────────────────────────
#> ✔ slurmtools.slurm_jon_template_path: /data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/slurm-job-bbi.tmpl
#> ✔ slurmtools.submission_root: /data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/submission-log
#> ✔ slurmtools.bbi_config_path: /data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/bbi.yaml
```

This block reads in a nonmem model with bbr and submits the job to
slurm:

``` r
mod_number <- "1001"
mod <- if (file.exists(file.path(nonmem, paste0(mod_number, ".yaml")))) {
  bbr::read_model(file.path(nonmem, mod_number))
} else {
  bbr::new_model(file.path(nonmem, mod_number)) 
}

submit_slurm_job(mod, overwrite = TRUE)
#> $status
#> [1] 0
#> 
#> $stdout
#> [1] "Submitted batch job 1166\n"
#> 
#> $stderr
#> [1] ""
#> 
#> $timeout
#> [1] FALSE
```

This block shows recently submitted and completed jobs:

``` r
knitr::kable(get_slurm_jobs(user = 'matthews'))
```

| job_id | partition | job_name | user_name | job_state | time | cpus | standard_input | standard_output | submit_time | start_time | end_time | current_working_directory |
|---:|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|
| 1162 | cpu2mem4gb | 1001-nonmem-run | matthews | PENDING | 00:00:00 | 1 | /dev/null | /tmp/RtmpFbHDVZ/Rbuild1aaecd670551dd/slurmtools/vignettes/model/nonmem/submission-log/slurm-1162.out | 2026-03-11 17:34:30 | 1970-01-01 | 1970-01-01 | /tmp/RtmpFbHDVZ/Rbuild1aaecd670551dd/slurmtools/vignettes/model/nonmem/submission-log |
| 1163 | cpu2mem4gb | 1001-nonmem-run | matthews | PENDING | 00:00:00 | 1 | /dev/null | /tmp/RtmpFbHDVZ/Rbuild1aaecd670551dd/slurmtools/vignettes/model/nonmem/submission-log/slurm-1163.out | 2026-03-11 17:34:30 | 1970-01-01 | 1970-01-01 | /tmp/RtmpFbHDVZ/Rbuild1aaecd670551dd/slurmtools/vignettes/model/nonmem/submission-log |
| 1164 | cpu2mem4gb | 1001-nonmem-run | matthews | PENDING | 00:00:00 | 1 | /dev/null | /tmp/RtmplLEXOp/file196ea54a3a4464/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-1164.out | 2026-03-11 17:35:00 | 1970-01-01 | 1970-01-01 | /tmp/RtmplLEXOp/file196ea54a3a4464/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 1165 | cpu2mem4gb | 1001-nonmem-run | matthews | PENDING | 00:00:00 | 1 | /dev/null | /tmp/RtmplLEXOp/file196ea54a3a4464/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-1165.out | 2026-03-11 17:35:00 | 1970-01-01 | 1970-01-01 | /tmp/RtmplLEXOp/file196ea54a3a4464/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 1166 | cpu2mem4gb | 1001-nonmem-run | matthews | PENDING | 00:00:00 | 1 | /dev/null | /data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/submission-log/slurm-1166.out | 2026-03-11 17:38:53 | 1970-01-01 | 1970-01-01 | /data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/submission-log |
