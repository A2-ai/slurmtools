
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmtools

<!-- badges: start -->
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

| job_id | job_state | cpus | partition | standard_input | standard_output | submit_time | start_time | end_time | user_name | current_working_directory |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
|  |  |  |  |  |  |  |  |  |  |  |
