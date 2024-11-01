
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/slurmtools/actions/workflows/RunChecks.yaml/badge.svg)](https://github.com/A2-ai/slurmtools/actions/workflows/RunChecks.yaml)
<!-- badges: end -->

`slurmtools` is a collection of utility functions suitable for
interacting with slurm and submitting nonmem jobs.

## Installation

You can install the development version of slurmtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("a2-ai/slurmtools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(slurmtools)
#> ── Needed slurmtools options ───────────────────────────────────────────────────
#> ✖ option('slurmtools.slurm_job_template_path') is not set.
#> ✖ option('slurmtools.submission_root') is not set.
#> ✖ option('slurmtools.bbi_config_path') is not set.
#> ℹ Please set all options for job submission defaults to work.
knitr::kable(get_slurm_jobs())
```

| job_id | partition | user_name | job_state | time | cpus | standard_input | standard_output | submit_time | start_time | end_time | current_working_directory |
|---:|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|
| 87 | cpu2mem4gb | matthews | FAILED | 2 secs | 1 | /dev/null | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log/slurm-87.out | 2024-11-01 15:20:24 | 2024-11-01 15:20:24 | 2024-11-01 15:20:26 | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log |
| 88 | cpu2mem4gb | matthews | COMPLETED | 1 secs | 1 | /dev/null | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log/slurm-88.out | 2024-11-01 15:20:24 | 2024-11-01 15:20:25 | 2024-11-01 15:20:26 | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log |
| 89 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log/slurm-89.out | 2024-11-01 15:20:25 | 2024-11-01 15:20:26 | 2024-11-01 15:20:26 | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log |
| 90 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log/slurm-90.out | 2024-11-01 15:20:25 | 2024-11-01 15:24:06 | 2024-11-01 15:24:06 | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log |
| 91 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log/slurm-91.out | 2024-11-01 15:20:26 | 2024-11-01 15:20:26 | 2024-11-01 15:20:26 | /tmp/Rtmpci7qIE/Rbuild1941e339490588/slurmtools/vignettes/model/nonmem/submission-log |
| 92 | cpu2mem4gb | matthews | FAILED | 1 secs | 1 | /dev/null | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-92.out | 2024-11-01 15:20:49 | 2024-11-01 15:24:06 | 2024-11-01 15:24:07 | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 93 | cpu2mem4gb | matthews | COMPLETED | 0 secs | 1 | /dev/null | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-93.out | 2024-11-01 15:20:50 | 2024-11-01 15:20:52 | 2024-11-01 15:20:52 | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 94 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-94.out | 2024-11-01 15:20:51 | 2024-11-01 15:20:52 | 2024-11-01 15:20:52 | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 95 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-95.out | 2024-11-01 15:20:52 | 2024-11-01 15:20:52 | 2024-11-01 15:20:52 | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
| 96 | cpu2mem4gb | matthews | FAILED | 0 secs | 1 | /dev/null | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log/slurm-96.out | 2024-11-01 15:20:54 | 2024-11-01 15:20:55 | 2024-11-01 15:20:55 | /tmp/Rtmp5NSlzS/file187f0067bd515b/slurmtools.Rcheck/vign_test/slurmtools/vignettes/model/nonmem/submission-log |
