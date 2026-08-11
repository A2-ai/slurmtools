# POC demo — template-first submit_slurm_job()
#
# Safe to run on the login node: every submission here is dry_run = TRUE, so
# nothing reaches sbatch. Run from the repo root:
#
#     /opt/R/4.5.3/bin/Rscript demo.R
#
# or source() it from an R session started at the repo root.

pkgload::load_all(".", quiet = TRUE)
demo_dir <- file.path(tempdir(), "slurmtools-poc-demo")
dir.create(demo_dir, showWarnings = FALSE)
setwd(demo_dir)

banner <- function(x) cat("\n", cli::rule(left = cli::style_bold(x)), "\n", sep = "")

## 1 — no required options anymore -------------------------------------------
banner("1. no options required")
cat(slurmtools_options_message())
# submission_root falls back to ./submission-log; setting the option still works.

## 2 — one-time setup: create the template for a workflow ---------------------
banner("2. create_slurm_template(): an R-script workflow")
create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
cat(brio::read_file("rscript.tmpl"))

## 3 — then submit files through it -------------------------------------------
banner("3. submit_slurm_job(file, template): dry run")
writeLines("x <- mean(rnorm(1e6))", "sim.R")
cmd <- submit_slurm_job(
  "sim.R",
  template = "rscript.tmpl",
  partition = "cpu2mem4gb",
  dry_run = TRUE
)
cat(cmd$template_script)

## 4 — a NONMEM/bbi workflow, with a parallel variant --------------------------
banner("4. a bbi workflow: parallel_args switch on when ncpu > 1")
create_slurm_template(
  "bbi-nonmem.tmpl",
  command = "bbi",
  args = c("nonmem", "run", "local", "{{file}}", "--config", "{{config_path}}"),
  parallel_args = c("--parallel", "--threads={{ncpu}}")
)
invisible(file.create("1001.mod"))
cmd <- submit_slurm_job(
  "1001.mod",
  template = "bbi-nonmem.tmpl",
  partition = "cpu2mem4gb",
  ncpu = 2,
  template_opts = list(config_path = "bbi.yaml"),
  dry_run = TRUE
)
cat(cmd$template_script)

## 5 — the template is a plain bash file the user owns -------------------------
banner("5. edit the template freely: add a notification")
tmpl <- readLines("rscript.tmpl")
writeLines(
  append(tmpl, 'curl -d "job {{job_name}} done" ntfy.sh/{{ntfy}}', after = length(tmpl)),
  "rscript.tmpl"
)
cmd <- submit_slurm_job(
  "sim.R",
  template = "rscript.tmpl",
  partition = "cpu2mem4gb",
  template_opts = list(ntfy = "my-channel"),
  dry_run = TRUE
)
cat(cmd$template_script)

## 6 — the guardrails -----------------------------------------------------------
banner("6. errors are early and actionable")
try(submit_slurm_job("sim.R", partition = "cpu2mem4gb", dry_run = TRUE))
try(create_slurm_template("nope.tmpl", command = "not-a-real-tool"))
try(submit_slurm_job("sim.R", template = "rscript.tmpl", partition = "not-a-partition", dry_run = TRUE))

banner("done — nothing was submitted")
