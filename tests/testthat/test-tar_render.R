test_that("tar_render() runs", {
  on.exit(unlink(c("_targets*", "report.*"), recursive = TRUE))
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "targets::tar_read(data)",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    tar_pipeline(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_render(report, "report.Rmd", quiet = TRUE)
    )
  })
  # First run.
  targets::tar_make(callr_function = NULL)
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  # Paths must be relative.
  expect_equal(out, c("report.html", "report.Rmd"))
  # Should not rerun the report.
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(targets::tar_progress()), 0L)
  targets::tar_script({
    library(tarchetypes)
    tar_pipeline(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_render(report, "report.Rmd")
    )
  })
  # Should rerun the report.
  targets::tar_make(callr_function = NULL)
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

tar_test("tar_render() on a nested report still runs from the project root", {
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "file.create(\"here\")",
    "```"
  )
  dir.create("out")
  writeLines(lines, file.path("out", "report.Rmd"))
  targets::tar_script({
    library(tarchetypes)
    tar_pipeline(
      tar_render(report, file.path("out", "report.Rmd"))
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
  targets::tar_make(callr_function = NULL)
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
})
