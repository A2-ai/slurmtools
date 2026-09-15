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

## 1 — no required options anymore -------------------------------------------
cat(slurmtools_options_message())
# submission_root falls back to ./submission-log; setting the option still works.

## 2 — one-time setup: create the template for a workflow ---------------------
create_slurm_template("rscript.tmpl", command = "Rscript", args = "{{file}}")
cat(brio::read_file("rscript.tmpl"))

## 3 — then submit files through it -------------------------------------------
writeLines("x <- mean(rnorm(1e6))", "sim.R")
cmd <- submit_slurm_job(
  "sim.R",
  template = "rscript.tmpl",
  partition = "cpu2mem4gb",
  dry_run = TRUE
)
cat(cmd$template_script)

## 4 — a NONMEM/bbi workflow, with a parallel variant --------------------------
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

##7 - generalizable ------------------------------------------------------------

create_slurm_template("python.tmpl", command = "python3", args = "{{file}}")
writeLines(c("with open('py-out.txt', 'w') as f:", "    f.write('this is what it sounds like when doves cry')"), "prince.py")
res <- submit_slurm_job("prince.py", template = "python.tmpl", partition = "cpu2mem4gb")
cat(res$stdout)

## 6 — the guardrails -----------------------------------------------------------
try(submit_slurm_job("sim.R", partition = "cpu2mem4gb", dry_run = TRUE))
try(create_slurm_template("nope.tmpl", command = "not-a-real-tool"))
try(submit_slurm_job("sim.R", template = "rscript.tmpl", partition = "not-a-partition", dry_run = TRUE))
