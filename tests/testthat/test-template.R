# --- Template() + with_sbatch() ---------------------------------------------

test_that("an empty Template renders to a bare bash shebang", {
  expect_equal(format(Template()), "#!/bin/bash")
})

test_that("with_sbatch adds one #SBATCH line per flag, in call order", {
  tmpl <- Template() |>
    with_sbatch("job-name") |>
    with_sbatch("cpus-per-task", "ncpu") |>
    with_sbatch("partition")

  expect_equal(
    format(tmpl),
    c(
      "#!/bin/bash",
      "#SBATCH --job-name={{job_name}}",
      "#SBATCH --cpus-per-task={{ncpu}}",
      "#SBATCH --partition={{partition}}"
    )
  )
})

test_that("the default placeholder is the flag with dashes as underscores", {
  tmpl <- Template() |> with_sbatch("ntasks-per-node")
  expect_equal(tmpl@sbatch, c("ntasks-per-node" = "ntasks_per_node"))
})

test_that("adding a flag twice replaces its placeholder in place", {
  tmpl <- Template() |>
    with_sbatch("nodes") |>
    with_sbatch("partition") |>
    with_sbatch("nodes", "n_nodes")
  expect_equal(tmpl@sbatch, c(nodes = "n_nodes", partition = "partition"))
})

test_that("flags are sbatch long options without dashes", {
  expect_error(Template() |> with_sbatch("--nodes"), "leading dashes")
  expect_error(Template() |> with_sbatch("Nodes"), "leading dashes")
  expect_error(
    Template() |> with_sbatch(c("nodes", "ntasks")),
    "single sbatch option"
  )
})

test_that("placeholder names must work as R argument names", {
  expect_error(
    Template() |> with_sbatch("cpus-per-task", "2cpu"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch("gres", "gpu-count"),
    "start with a letter"
  )
  expect_error(
    Template() |> with_sbatch("gres", c("a", "b")),
    "single placeholder name"
  )
})

test_that("with_sbatch needs a Template", {
  expect_error(with_sbatch("rscript.tmpl", "partition"), "must be a Template")
})

test_that("Template() validates flags passed directly", {
  expect_error(Template(sbatch = c("a", "b")), "named character vector")
  expect_error(Template(sbatch = c(nodes = "n", nodes = "m")), "once")
})

test_that("print shows the rendered header", {
  expect_output(
    print(Template() |> with_sbatch("partition")),
    "#SBATCH --partition={{partition}}",
    fixed = TRUE
  )
})
