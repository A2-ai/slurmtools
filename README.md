slurmtools
==================

`slurmtools` is a collection of utility functions suitable for interacting with slurm and submitting nonmem jobs.

## Usage

``` r
> library(slurmtools)
> get_slurm_jobs()
# A tibble: 4 × 10
  job_id job_state    cpus partition  standard_input standard_output                                                                  submit_time         start_time          user_name current_working_dire…¹
   <int> <chr>       <int> <chr>      <chr>          <chr>                                                                            <dttm>              <dttm>              <chr>     <chr>                 
1      3 COMPLETED       1 cpu2mem4gb /dev/null      /cluster-data/user-homes/devin/test-training/scm_dir1/base_modelfit_dir1/NM_run… 2024-02-20 17:19:30 2024-02-20 17:23:15 devin     /cluster-data/user-ho…
2      4 RUNNING         1 cpu2mem4gb /dev/null      /cluster-data/user-homes/devin/test-training/scm_dir1/modelfit_dir1/NM_run1/nmf… 2024-02-20 17:24:03 2024-02-20 17:24:03 devin     /cluster-data/user-ho…
3      5 RUNNING         1 cpu2mem4gb /dev/null      /cluster-data/user-homes/devin/test-training/scm_dir1/modelfit_dir1/NM_run2/nmf… 2024-02-20 17:24:03 2024-02-20 17:24:03 devin     /cluster-data/user-ho…
4      6 CONFIGURING     1 cpu2mem4gb /dev/null      /cluster-data/user-homes/devin/test-training/scm_dir1/modelfit_dir1/NM_run3/nmf… 2024-02-20 17:24:03 2024-02-20 17:24:03 devin     /cluster-data/user-ho…
# ℹ abbreviated name: ¹​current_working_directory
```
