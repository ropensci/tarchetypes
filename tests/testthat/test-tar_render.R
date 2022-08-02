targets::tar_test("tar_render() works", {
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
      tar_render(report, "report.Rmd", quiet = TRUE)
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  expect_equal(basename(out), c("report.html", "report.Rmd"))
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
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
  skip_rmarkdown()
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
  skip_rmarkdown()
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

targets::tar_test("tar_render() works with child documents", {
  skip_on_cran()
  skip_rmarkdown()
  # Create a main file and a child file in a subdirectory
  dir.create("report")
  writeLines(
    text = c(
      "---",
      "title: report",
      "output_format: html_document",
      "---",
      "",
      "```{r, child = \"report/child.Rmd\"}",
      "```",
      "",
      "```{r}",
      "targets::tar_read(main)",
      "```"
    ),
    con = "report/main.Rmd"
  )
  writeLines(
    text = c(
      "# Child Document",
      "",
      "```{r}",
      "targets::tar_read(child)",
      "```"
    ),
    con = "report/child.Rmd"
  )
  targets::tar_script({
    library(targets)
    library(tarchetypes)
    list(
      tar_target(main, "value_main_target"),
      tar_target(child, "value_child_target"),
      tar_render(report, "report/main.Rmd", quiet = TRUE)
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("child", "main", "report")))
  out <- targets::tar_read(report)
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(basename(out), c("main.html", "main.Rmd"))
  } else {
    expect_equal(out, c("report/main.html", "report/main.Rmd"))
  }
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  # Should rerun the report.
  # Only the dependency in the main document is changed.
  targets::tar_script({
    library(targets)
    library(tarchetypes)
    list(
      tar_target(main, "value_main_target_changed"),
      tar_target(child, "value_child_target"),
      tar_render(report, "report/main.Rmd", quiet = TRUE)
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    sort(targets::tar_progress()$name),
    sort(c("child", "main", "report"))
  )
  # Should rerun the report.
  # Only the dependency in the child document is changed.
  targets::tar_script({
    library(targets)
    library(tarchetypes)
    list(
      tar_target(main, "value_main_target_changed"),
      tar_target(child, "value_child_target_changed"),
      tar_render(report, "report/main.Rmd", quiet = TRUE)
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    sort(targets::tar_progress()$name),
    sort(c("child", "main", "report"))
  )
  # Should rerun the report.
  # Change the main file slightly (but not the code)
  writeLines(
    text = c(
      "---",
      "title: A new report",
      "output_format: html_document",
      "---",
      "",
      "```{r, child = \"report/child.Rmd\"}",
      "```",
      "",
      "```{r}",
      "targets::tar_read(main)",
      "```"
    ),
    con = "report/main.Rmd"
  )
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    sort(targets::tar_progress()$name),
    sort(c("child", "main", "report"))
  )
  # Should rerun the report.
  # Change the child file slightly (but not the code)
  writeLines(
    text = c(
      "# A New Child Document",
      "",
      "```{r}",
      "targets::tar_read(child)",
      "```"
    ),
    con = "report/child.Rmd"
  )
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    sort(targets::tar_progress()$name),
    sort(c("child", "main", "report"))
  )
  # Detect whether `value_main_target_changed` and
  # `value_child_target_changed` are correctly print in HTML file
  # (the values should occure once in the HTML file)
  html_file <- readLines("report/main.html")
  expect_identical(
    sum(grepl("value_main_target_changed", html_file, fixed = TRUE)),
    1L
  )
  expect_identical(
    sum(grepl("value_child_target_changed", html_file, fixed = TRUE)),
    1L
  )
  # Check that the dependency graph is correct of our targets. `report` should
  # depend on `main` and `child`.
  edges <- tar_network(callr_function = NULL)$edges
  edges <- edges[edges$to == "report",, drop = FALSE] # nolint
  expect_identical(
    edges,
    tibble::tibble(
      from = c("child", "main"),
      to = c("report", "report")
    )
  )
})
