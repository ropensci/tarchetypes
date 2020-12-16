# Cannot use targets::tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_render_raw() works", suppressMessages({
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
      tar_render_raw("report", "report.Rmd", quiet = TRUE)
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
      tar_render_raw("report", "report.Rmd")
    )
  })
  # Should rerun the report.
  targets::tar_make(callr_function = NULL)
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
}))

# Cannot use targets::tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_render_raw(nested) runs from project root", suppressMessages({
  on.exit(
    unlink(
      c("_targets*", "report.*", "out_tar_render", "here"),
      recursive = TRUE
    )
  )
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
    tar_pipeline(
      tar_render_raw("report", file.path("out_tar_render", "report.Rmd"))
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_render", "here")))
  targets::tar_make(callr_function = NULL)
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_render", "here")))
}))

# Cannot use targets::tar_test() here because of relative path issues on Windows. # nolint
test_that("tar_render_raw() for parameterized reports", suppressMessages({
  on.exit(unlink(c("_targets*", "report.*"), recursive = TRUE))
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
    tar_pipeline(
      tar_target(upstream, "anotherverydistinctvalue"),
      tar_render_raw(
        "report",
        "report.Rmd",
        render_arguments = quote(list(params = list(param2 = upstream)))
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  lines <- readLines("report.html")
  expect_true(any(grepl("anotherverydistinctvalue", lines)))
}))
