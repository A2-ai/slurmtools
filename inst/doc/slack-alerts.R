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
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slurmtools)
library(bbr)
library(here)

nonmem = file.path(here::here(), "vignettes", "model", "nonmem")
options('slurmtools.submission_root' = file.path(nonmem, "submission-log"))

## -----------------------------------------------------------------------------
mod_number <- "1001"

if (file.exists(file.path(nonmem, paste0(mod_number, ".yaml")))) {
  mod <- bbr::read_model(file.path(nonmem, mod_number))
} else {
  mod <- bbr::new_model(file.path(nonmem, mod_number))
}

## -----------------------------------------------------------------------------
Sys.which("slack_notifier")

## -----------------------------------------------------------------------------
slurmtools::generate_nmm_config( 
  mod, 
  alert = "slack",
  email = "matthews@a2-ai.com",
  watched_dir = "/cluster-data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem",
  output_dir = "/cluster-data/user-homes/matthews/Packages/slurmtools/vignettes/model/nonmem/in_progress")

## -----------------------------------------------------------------------------
submission_nmm <- slurmtools::submit_nonmem_model( 
  mod, 
  overwrite = TRUE,
  slurm_job_template_path = file.path(nonmem, "slurm-job-nmm.tmpl"),
  slurm_template_opts = list(
    nmm_exe_path = normalizePath("~/.local/bin/nmm")
  )
)

submission_nmm

## -----------------------------------------------------------------------------
slurmtools::get_slurm_jobs()

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

