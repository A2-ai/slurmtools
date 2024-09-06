## ----include = FALSE----------------------------------------------------------
#removing generated files from running this vignette
nonmem <- file.path("model", "nonmem")

unlink(file.path(nonmem, "1001"), recursive = TRUE)
unlink(file.path(nonmem, "1001.yaml"))
unlink(file.path(nonmem, "1001.toml"))
unlink(file.path(nonmem, "submission-log"), recursive = TRUE) 
unlink(file.path(nonmem, "in_progress"), recursive = TRUE)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "" 
)

## ----setup--------------------------------------------------------------------
library(slurmtools)

## -----------------------------------------------------------------------------
Sys.which("bbi")

## -----------------------------------------------------------------------------
library(bbr)
library(here)

nonmem = file.path(here::here(), "vignettes", "model", "nonmem")

options('slurmtools.submission_root' = file.path(nonmem, "submission-log"))
options('slurmtools.bbi_config_path' = file.path(nonmem, "bbi.yaml"))

## -----------------------------------------------------------------------------
mod_number <- "1001"

if (file.exists(file.path(nonmem, paste0(mod_number, ".yaml")))) {
  mod <- bbr::read_model(file.path(nonmem, mod_number))
} else {
  mod <- bbr::new_model(file.path(nonmem, mod_number))
}

## -----------------------------------------------------------------------------
submission <- slurmtools::submit_nonmem_model(
  mod,
  slurm_job_template_path = file.path(nonmem, "slurm-job-bbi.tmpl"),
)

submission

## -----------------------------------------------------------------------------
slurmtools::get_slurm_jobs(user = 'matthews')

## -----------------------------------------------------------------------------
submission_ntfy <- slurmtools::submit_nonmem_model(
  mod, 
  slurm_job_template_path = file.path(nonmem, "slurm-job-bbi-ntfy.tmpl"),
  overwrite = TRUE,
  slurm_template_opts = list(
    ntfy = "ntfy_demo")
)

submission_ntfy

## ----include = FALSE----------------------------------------------------------
#cancelling any running nonmem jobs
state <- slurmtools::get_slurm_jobs(user = "matthews")

if (any(state$job_state %in% c("RUNNING", "CONFIGURING"))) {
  for (job_id in state %>% dplyr::filter(job_state == "RUNNING") %>% dplyr::pull("job_id")) {
    processx::run("scancel", args = paste0(job_id))
  }
}

#removing generated files from running this vignette
nonmem <- file.path("model", "nonmem")

unlink(file.path(nonmem, "1001"), recursive = TRUE)
unlink(file.path(nonmem, "1001.yaml"))
unlink(file.path(nonmem, "1001.toml"))
unlink(file.path(nonmem, "submission-log"), recursive = TRUE) 
unlink(file.path(nonmem, "in_progress"), recursive = TRUE)

