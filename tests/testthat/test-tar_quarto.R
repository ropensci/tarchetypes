targets::tar_test("tar_quarto() works", {
  skip_quarto()
  lines <- c(
    "---",
    "title: report",
    "output_format: html",
    "---",
    "",
    "```{r}",
    "targets::tar_read(data)",
    "```"
  )
  writeLines(lines, "report.qmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_quarto(report, input = "report.qmd", files = "report.html")
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  expect_equal(sort(basename(out)), sort(c("report.html", "report.qmd")))
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_quarto(report, input = "report.qmd", files = "report.html")
    )
  })
  # Should rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

targets::tar_test("tar_quarto(nested) runs from the project root", {
  skip_quarto()
  lines <- c(
    "---",
    "title: report",
    "output_format: html",
    "---",
    "",
    "```{r}",
    "file.create(\"here\")",
    "```"
  )
  dir.create("out_tar_quarto")
  writeLines(lines, file.path("out_tar_quarto", "report.qmd"))
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_quarto(
        report,
        input = file.path("out_tar_quarto", "report.qmd"),
        files = file.path("out_tar_quarto", "report.html")
      )
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_quarto", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_quarto", "here")))
})

targets::tar_test("tar_quarto() for parameterized reports", {
  skip_quarto()
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
  writeLines(lines, "report.qmd")
  targets::tar_script({
    library(tarchetypes)
    value <- "abcd1234verydistinctvalue"
    list(
      tar_target(upstream, "anotherverydistinctvalue"),
      tar_quarto(
        report,
        input = "report.qmd",
        files = "report.html",
        execute_params = list(param1 = !!value, param2 = upstream)
      )
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  lines <- readLines("report.html", warn = FALSE)
  expect_true(any(grepl("anotherverydistinctvalue", lines)))
})

targets::tar_test("tar_quarto() works with child documents", {
  skip_on_cran()
  skip_quarto()
  # Create a main file and a child file in a subdirectory
  dir.create("report")
  writeLines(
    text = c(
      "---",
      "title: report",
      "output_format: html_document",
      "---",
      "",
      "```{r, child = \"report/child.qmd\"}",
      "```",
      "",
      "```{r}",
      "targets::tar_read(main)",
      "```"
    ),
    con = "report/main.qmd"
  )
  writeLines(
    text = c(
      "# Child Document",
      "",
      "```{r}",
      "targets::tar_read(child)",
      "```"
    ),
    con = "report/child.qmd"
  )
  targets::tar_script({
    library(targets)
    library(tarchetypes)
    list(
      tar_target(main, "value_main_target"),
      tar_target(child, "value_child_target"),
      tar_quarto(report, input = "report/main.qmd", files = "report/main.html")
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("child", "main", "report")))
  out <- targets::tar_read(report)
  expect_equal(sort(out), sort(c("report/main.html", "report/main.qmd")))
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
      tar_quarto(report, input = "report/main.qmd", files = "report/main.html")
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
      tar_quarto(report, input = "report/main.qmd", files = "report/main.html")
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
      "```{r, child = \"report/child.qmd\"}",
      "```",
      "",
      "```{r}",
      "targets::tar_read(main)",
      "```"
    ),
    con = "report/main.qmd"
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
    con = "report/child.qmd"
  )
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(
    sort(targets::tar_progress()$name),
    sort(c("child", "main", "report"))
  )
  # Detect whether `value_main_target_changed` and
  # `value_child_target_changed` are correctly print in HTML file
  # (the values should occure once in the HTML file)
  html_file <- readLines("report/main.html", warn = FALSE)
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
