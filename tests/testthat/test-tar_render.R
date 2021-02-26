targets::tar_test("tar_render() works", {
  skip_pandoc()
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
    list(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_render(report, "report.Rmd", quiet = TRUE)
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  expect_equal(basename(out), c("report.html", "report.Rmd"))
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(nrow(targets::tar_progress()), 0L)
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_render(report, "report.Rmd")
    )
  })
  # Should rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

targets::tar_test("tar_render(nested) runs from the project root", {
  skip_pandoc()
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
  dir.create("out_tar_render")
  writeLines(lines, file.path("out_tar_render", "report.Rmd"))
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_render(report, file.path("out_tar_render", "report.Rmd"))
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_render", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_render", "here")))
})

targets::tar_test("tar_render() for parameterized reports", {
  skip_pandoc()
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "params:",
    "  param1: \"default\"",
    "  param2: \"default\"",
    "---",
    "```{r}",
    "print(params$param1)",
    "print(params$param2)",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    value <- "abcd1234verydistinctvalue"
    list(
      tar_target(upstream, "anotherverydistinctvalue"),
      tar_render(
        report,
        "report.Rmd",
        params = list(param1 = !!value, param2 = upstream)
      )
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  lines <- readLines("report.html")
  expect_true(any(grepl("anotherverydistinctvalue", lines)))
})

targets::tar_test("tar_render() with a _files/ directory", {
  skip_pandoc()
  lines <- c(
    "---",
    "title: report with a plot",
    "output_format: html_document",
    "---",
    "```{r}",
    "plot(seq_len(4))",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    list(tar_render(report, "report.Rmd", clean = FALSE))
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    basename(targets::tar_read(report)),
    c("report.html", "report.Rmd", "report_files")
  )
})
