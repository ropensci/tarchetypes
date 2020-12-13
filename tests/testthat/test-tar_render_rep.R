tar_test("tar_render_rep() manifest", {
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "params:",
    "  par: \"default value\"",
    "---",
    "",
    "```{r}",
    "print(params$par)",
    "print(tar_read(x))",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    tar_pipeline(
      tar_target(x, "value_of_x"),
      tar_render_rep(
        report,
        "report.Rmd",
        params = quote(tibble(par = c("par_value_1", "par_value_2")))
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 3L)
  expect_equal(sort(out$name), sort(c("x", "report_params", "report")))
  expect_equal(sum(is.na(out$pattern)), 2L)
  expect_equal(out$pattern[out$name == "report"], "map(report_params)")
})
