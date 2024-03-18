targets::tar_test("tar_knit() works", {
  skip_rmarkdown()
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
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  expect_equal(basename(out), c("report.md", "report.Rmd"))
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_knit(report, "report.Rmd")
    )
  })
  # Should rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

targets::tar_test("tar_knit() warns about tar_read_raw()", {
  skip_rmarkdown()
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "targets::tar_read_raw('data')",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  expect_warning(
    suppressMessages(targets::tar_make(callr_function = NULL)),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_knit() warns about tar_load_raw()", {
  skip_rmarkdown()
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "envir <- new.env(parent = emptyenv())",
    "targets::tar_load_raw('data', envir = envir)",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  expect_warning(
    suppressMessages(targets::tar_make(callr_function = NULL)),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_knit(nested) runs from project root", {
  skip_rmarkdown()
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
    list(
      tar_knit(report, file.path("out", "report.Rmd"))
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
})

targets::tar_test("tar_knit() custom output_file & working directory", {
  skip_on_cran()
  skip_rmarkdown()
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "tar_read(upstream, store = '../_targets')",
    "file.create(\"here\")",
    "```"
  )
  dir.create("out")
  on.exit(unlink("out", recursive = TRUE))
  writeLines(lines, file.path("out", "report.Rmd"))
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(upstream, "UPSTREAM_SUCCEEDED"),
      tar_knit(
        name = report,
        path = file.path("out", "report.Rmd"),
        output_file = file.path("out", "report.md"),
        working_directory = "out"
      )
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_false(file.exists("here"))
  expect_true(file.exists(file.path("out", "here")))
  lines <- readLines(file.path("out", "report.md"))
  expect_true(any(grepl("UPSTREAM_SUCCEEDED", lines)))
  expect_equal(
    sort(targets::tar_read(report)),
    sort(c(file.path("out", c("report.md", "report.Rmd"))))
  )
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0L))
})
