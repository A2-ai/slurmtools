# Extracted from test-launcher.R:46

# prequel ----------------------------------------------------------------------
rscript <- "/opt/R/4.5.3/bin/Rscript"

# test -------------------------------------------------------------------------
tmpl <- Template() |>
    with_job_name() |>
    with_command("echo", "starting", launcher = "env") |>
    with_command(rscript, "{{file}}")
expect_match(format(tmpl)[length(format(tmpl)) - 1], "^env ")
