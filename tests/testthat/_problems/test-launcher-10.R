# Extracted from test-launcher.R:10

# prequel ----------------------------------------------------------------------
rscript <- "/opt/R/4.5.3/bin/Rscript"

# test -------------------------------------------------------------------------
tmpl <- Template() |> with_job_name() |> with_command(rscript, "{{file}}", launcher = "env")
expect_match(
    utils::tail(format(tmpl), 1),
    sprintf("^%s %s \\\\{\\\\{file\\\\}\\\\}$", Sys.which("env"), rscript)
  )
