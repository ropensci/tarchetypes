# Cannot use tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_knit() works", suppressMessages({
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
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  # First run.
  targets::tar_make(callr_function = NULL)
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  # Paths must be relative.
  expect_equal(out, c("report.md", "report.Rmd"))
  # Should not rerun the report.
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(targets::tar_progress()), 0L)
  targets::tar_script({
    library(tarchetypes)
    tar_pipeline(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_knit(report, "report.Rmd")
    )
  })
  # Should rerun the report.
  targets::tar_make(callr_function = NULL)
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
}))

# Cannot use tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_knit() warns about tar_read_raw()", suppressMessages({
  on.exit(unlink(c("_targets*", "report.*"), recursive = TRUE))
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
    tar_pipeline(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  expect_warning(
    targets::tar_make(callr_function = NULL),
    class = "condition_validate"
  )
}))

# Cannot use tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_knit() warns about tar_load_raw()", suppressMessages({
  on.exit(unlink(c("_targets*", "report.*"), recursive = TRUE))
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
    tar_pipeline(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_knit(report, "report.Rmd", quiet = TRUE)
    )
  })
  expect_warning(
    targets::tar_make(callr_function = NULL),
    class = "condition_validate"
  )
}))

# Cannot use tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_knit(nested) runs from project root", suppressMessages({
  on.exit(unlink(c("_targets*", "report.*", "out", "here"), recursive = TRUE))
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
      tar_knit(report, file.path("out", "report.Rmd"))
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
  targets::tar_make(callr_function = NULL)
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out", "here")))
}))
