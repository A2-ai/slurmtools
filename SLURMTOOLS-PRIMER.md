# slurmtools primer: slurm, the old build, the new build, and why

This is for someone joining the project who is new to slurm, new to slurmtools,
or both. It explains what the package is for, how the version on `main` works,
why it is being redesigned, and how the redesign on `poc/template-first-submit`
works. If you want to see the two builds side by side, read
`main-vs-template-builder.qmd` next: it runs the same R input through both
builds and diffs the job scripts they produce.

---

## 1. Slurm in five minutes

**The problem slurm solves.** A cluster is a lot of machines shared by a lot of
people. You don't run heavy work on the machine you log in to. You describe the
work, and a scheduler finds a machine for it. Slurm is that scheduler.

| Term | What it means here |
|---|---|
| **login node** | The machine you SSH or RStudio into. This is where you write code and submit work, not where heavy work runs. |
| **compute node** | A machine that runs the work. On our cluster these are cloud VMs that start on demand, so a job can wait a few minutes while its node boots. |
| **partition** | A named pool of identical nodes. Ours are named by size: `cpu2mem4gb` means 2 CPUs and 4 GB per node, `cpu4mem32gb` means 4 CPUs and 32 GB. `sinfo` lists them. |
| **job** | One unit of submitted work. It gets a numeric **job id** (e.g. `2051`). |
| **job script** | A bash script whose top lines are `#SBATCH` comments. These are requests to slurm: which partition, how many CPUs, what name, where the logs go. Below them come the commands to run. |
| **`sbatch`** | The command that submits a job script. It prints the job id. |
| **`squeue`** | Shows jobs that are waiting or running. It forgets a job a few minutes after it finishes. |
| **`sacct`** | The accounting history. It still knows finished jobs, including their state (COMPLETED, FAILED, CANCELLED, …), exit code and run time. |
| **`scancel`** | Cancels a job. |
| **logs** | The job's stdout and stderr go to files given by `--output` and `--error`. `%j` in the name becomes the job id. |

A complete job script looks like this:

```bash
#!/bin/bash
#SBATCH --job-name=sim
#SBATCH --partition=cpu2mem4gb
#SBATCH --cpus-per-task=2
#SBATCH --output=submission-log/sim-%j.out

/usr/local/bin/Rscript sim.R
```

`sbatch sim.sh` queues it. Slurm starts a `cpu2mem4gb` node, runs the script
there, and writes what R prints to `submission-log/sim-2051.out`.

Two things about this cluster that surprise people:

- **Use absolute paths to programs.** The compute node runs the script with its
  own environment. If `Rscript` resolves to a different R on the node, you get
  subtle breakage. slurmtools writes the full path it found on the login node
  (`Sys.which()`) into the script.
- **Don't submit from `/tmp`.** It is local to each machine. Work under
  `$HOME`, which every node can see.

## 2. What slurmtools is

slurmtools is an R package that saves scientists from writing that bash file
and running those commands by hand. From R, you say what to run and with what
resources. The package does the rest:

1. **writes** the job script (from a template, filling in the blanks),
2. **checks** the request before it wastes a queue slot (does the partition
   exist? does it have that many CPUs?),
3. **submits** it with `sbatch`,
4. helps you **follow** the job (status, logs, cancel).

The template engine is [whisker](https://github.com/edwindj/whisker) (mustache
syntax). `{{ncpu}}` is replaced by a value, and `{{#parallel}} … {{/parallel}}`
is kept only if `parallel` is true.

## 3. The old build: `main`

`main` grew out of one job: running **NONMEM models through `bbi`** (the
pharmacometrics estimation tool and its runner). Its API has that shape.

```r
options(
  slurmtools.slurm_job_template_path = "model/nonmem/slurm-job-bbi.tmpl",
  slurmtools.bbi_config_path         = "model/nonmem/bbi.yaml",
  slurmtools.submission_root         = "submission-log"
)
submit_slurm_job(mod, partition = "cpu2mem4gb", ncpu = 2)   # mod: a path or a bbr model
```

The template is a bash file kept in the project. `submit_slurm_job()` fills it
from a **fixed list** of variables: `model_path`, `bbi_exe_path`,
`bbi_config_path`, `project_path`, `project_name`, `job_name`, `partition`,
`ncpu`, `parallel`. Any extra variable goes in `slurm_template_opts = list(...)`.
The call returns the raw result of running `sbatch`.

**What was good about it:** you set it up once per project and then submit with
one line. The template is ordinary bash, so anything bash can do is possible.

**What went wrong as more people used it:**

| Problem | Why it matters |
|---|---|
| NONMEM-shaped vocabulary (`.mod`, `model_path`, `bbi_*`) | Running an R script, Monolix or Python means pretending the script is a "model". The job name comes out as `sim-nonmem-run`. |
| A typo in a template variable renders as **empty text**, silently | You get `nonmem run local .mod --config` and the job fails on the node, minutes later, with a confusing error. |
| No `--output`/`%j` in the header | Slurm's default `slurm-<id>.out` lands wherever. A pinned name gets overwritten on resubmit. |
| The job id comes back as text inside `sbatch`'s stdout | You have to parse it yourself to follow the job. |
| Status via `squeue` only | Finished jobs vanish after about 5 minutes, so "did it succeed?" has no answer. |
| Anything extra (a notification, a symlink to the result) means editing the template | Every project ends up with its own slightly different copy. |
| Only the partition and CPU count are checked | Asking for too much memory, a GPU on a CPU partition, or a wasteful multi-node layout gets through. |

## 4. Why a new build

- **Client need (CLIENT-1):** submit plain `Rscript` jobs from the login node,
  and run Monolix jobs. Neither fits the NONMEM-shaped API.
- **2026-09-01 design meeting:** agreed to drop the S3-generic approach
  (dispatching on "what kind of engine is this?" confused people). A
  template should be *a command plus settings*, with sbatch options as a
  **first-class data structure** (known flags, required vs optional,
  conditionals). New requirements came out of it: pre- and post-run hooks,
  per-engine parallel control, and "current users must not be broken".
- **Client template corpus (2026-09-01/02):** ten real job scripts from
  clients were collected and analysed (`TEMPLATE-CORPUS.md`). They showed
  what is actually needed. Parallelism is a topology (nodes × tasks ×
  CPUs-per-task), not one `ncpu` number. The command is often a launcher plus
  a binary (`mpirun … distMonolix`). Hooks come in three shapes (provenance
  before, banners, events after). Derived paths such as a model's directory or
  file stem are needed too.
- **2026-09-04 decision:** a **builder-pattern template object**, validated
  with **S7** (R's newer class system; its properties carry validators, so a
  bad template can't even be constructed).

## 5. The new build: `poc/template-first-submit`

### The pieces

| Piece | What it is |
|---|---|
| `default_template()` | The usual starting point: main's header shape (`--job-name`, `--nodes=1`, `--ntasks=1`, `--cpus-per-task` from `ncpu`, `--partition`, and `--account` only if given). Add a command and you can submit. |
| `Template()` | An empty template, for building from scratch. An S7 object holding the `#SBATCH` flags, the command, hook lines and the values filled so far. It prints as the script it would become. |
| `with_<flag>()` | Adds one `#SBATCH` line. There are ten named verbs (`with_job_name`, `with_partition`, `with_account`, `with_cpus_per_task`, `with_nodes`, `with_ntasks`, `with_ntasks_per_node`, `with_gres`, `with_output`, `with_error`), plus the **blanket** `with_sbatch(t, "<any flag>")` for everything else (`time`, `mem`, …). Each verb takes a placeholder name and `optional = TRUE` to drop the line when it is left unfilled. |
| `with_command()` | What to run. The program is resolved to an absolute path. `launcher =` puts e.g. `mpirun` in front. |
| `with_pre_run()` / `with_post_run()` | Plain bash lines before and after the command. |
| `fill()` | Supplies placeholder values early. Misspelling a placeholder is an **error** that lists the real ones. |
| `submit_slurm_job(template, …)` | The final fills, then checks, then `sbatch`. It refuses if a required placeholder is still empty and names it. |
| `Job` | The S7 handle `submit_slurm_job()` returns: id, name, partition, script path, log paths. |
| `slurm_job_status()`, `wait_for_slurm_job()`, `slurm_job_log()`, `cancel_slurm_job()` | Follow the job through its handle, using `sacct`, so finished jobs are still visible. |
| `write_slurm_template()` | Saves a Template as a whisker file, for people who want a file in the repo. |

### A complete example

```r
library(slurmtools)

rscript <- default_template() |>
  with_command("Rscript", "{{file}}") |>
  with_sbatch("time") |>                  # any flag the default lacks
  with_post_run('echo "done: exit $?"')

rscript                                   # prints the script with {{placeholders}}

job <- submit_slurm_job(rscript, file = "sim.R", time = "01:00:00",
                        partition = "cpu2mem4gb", ncpu = 2)   # job name: "sim", from the file
job                                       # <Job> 2051 · sim · cpu2mem4gb …
wait_for_slurm_job(job)                   # polls sacct until the job finishes
slurm_job_log(job)                        # what R printed
```

The template is built **once** and submitted many times with different fills,
which is the build-once, reuse-everywhere idea.

### What the package does for you at submit

- Names the job after the file (`sim.R` → `sim`) when you don't give `job_name`.
- Adds `--output` / `--error` as `<submission_root>/<job_name>-%j.out/.err`
  if you didn't, so resubmits never overwrite a log.
- Derives `{{file_dir}}`, `{{file_stem}}`, `{{file_ext}}` from `file`, `{{log_dir}}`
  from the submission root, and `{{parallel}}` (true when cpus-per-task > 1).
  This covers the common cases that whisker, which has no filters, can't
  express.
- Checks the request against the partition (from `sinfo`): the CPU count, that
  `--mem` fits on one node, that `--gres` exists there, and a warning when
  `--nodes > 1` would reserve whole nodes and leave cores idle.

### Design choices and the reason for each

| Choice | Reason |
|---|---|
| Builder pattern instead of a hand-edited bash file | The flags become data the package can validate, drop when optional, and check against the partition. |
| S7 classes (`Template`, `Job`) | Validation lives on the object, so an invalid template can't exist, whichever verb built it. |
| Template and Job are separate classes | A template is reusable and has no id. A job is one submission with an id. |
| Named verbs for ten flags, `with_sbatch()` for the rest | The ten are the ones the client corpus uses. Everything else stays possible without waiting for a new verb. |
| whisker kept | Existing templates and users know it. The derived builtins remove the one case (dirname/stem) that seemed to demand a richer engine. |
| Hooks are plain lines; ntfy is a recipe, not a feature | Slurm's own mail does not deliver on this cluster. Posting to ntfy.sh with `curl` in a post-run line works, and it isn't special-cased. |
| One default template, main's header shape | People start from what they already know. Anything more is one `with_*()` call. |
| Old calls keep working through a shim | `R/legacy.R` is main's function verbatim. A main-style call is recognised, re-matched against main's own signature, and warned about once per session. Projects upgrade without edits. |
| Program paths resolved on the login node; no `exec` | The compute node runs exactly what you tested with. |

## 6. Where it stands (2026-09-23)

**Built and suite-tested,** each slice also run once for real on the cluster:
the builder, the named and blanket verbs, fills, optional flags, the command
and launcher, the `Job` handle, status, wait, log and cancel, hooks v1, the
derived builtins, and the topology checks.
`default_template()` and the legacy shim are suite-tested and dry-run only;
no real job has been submitted through them yet.

**Not done or open:**

- **The file form doesn't check for unfilled placeholders** (PLAN § 6.2).
  main's template passed through the *new* file form still renders empty
  fields silently. A main-style call is safe, because the shim catches it.
- Rename `fill()`? It masks `tidyr::fill` for tidyverse users.
- The README and vignette still describe the file form. What happens to
  `create_slurm_template()` and the file form?
- Hooks v2 (success-only / failure-only, via a bash trap), batch submission,
  and `slurm_job_efficiency()`.

## 7. Where to read next

| To… | Read |
|---|---|
| See old vs new on the same input | `main-vs-template-builder.qmd` (render it) |
| Try every new function, one chunk each | `template-builder-walkthrough.qmd` |
| See the decisions and their dates | `DESIGN-SBATCH-OPTIONS.md`, § "Open after the meeting" (`✅ LANDED` notes) |
| See the full roadmap (handle, lifecycle, hooks, batch, guardrails) | `PLAN-LAYERS-ON-SBATCH.md` |
| See the client job scripts that drove the design | `TEMPLATE-CORPUS.md` |
| Read the function reference | `?Template`, `?with_sbatch`, `?submit_slurm_job`, `?Job`, `?template_builtins` |
