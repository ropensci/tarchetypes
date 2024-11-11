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
      tar_quarto(report, path = "report.qmd")
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  out <- setdiff(out, "report_files")
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
      tar_quarto(report, path = "report.qmd")
    )
  })
  # Should rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

targets::tar_test("tar_quarto(nested) runs from the project root", {
  skip_on_cran()
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
        path = file.path("out_tar_quarto", "report.qmd")
      )
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_quarto", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_true(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_quarto", "here")))
})

targets::tar_test("tar_quarto() custom working dir", {
  skip_on_cran()
  skip_rmarkdown()
  lines <- c(
    "---",
    "title: report",
    "output_format: html",
    "---",
    "",
    "```{r}",
    "targets::tar_read(upstream, store = '../_targets')",
    "file.create(\"here\")",
    "```"
  )
  dir.create("out_tar_quarto")
  on.exit(unlink("out_tar_quarto", recursive = TRUE))
  writeLines(lines, file.path("out_tar_quarto", "report.qmd"))
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(upstream, "UPSTREAM_SUCCEEDED"),
      tar_quarto(
        name = report,
        path = file.path("out_tar_quarto", "report.qmd"),
        working_directory = "out_tar_quarto"
      )
    )
  })
  expect_false(file.exists("here"))
  expect_false(file.exists(file.path("out_tar_quarto", "here")))
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_false(file.exists("here"))
  expect_true(file.exists(file.path("out_tar_quarto", "here")))
  lines <- readLines(file.path("out_tar_quarto", "report.html"), warn = FALSE)
  expect_true(any(grepl("UPSTREAM_SUCCEEDED", lines)))
  expect_equal(
    sort(targets::tar_read(report)),
    sort(c(file.path("out_tar_quarto", c("report.html", "report.qmd"))))
  )
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0L))
})

targets::tar_test("tar_quarto() for parameterized reports", {
  skip_on_cran()
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
        path = "report.qmd",
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
      tar_quarto(report, path = "report/main.qmd")
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
      tar_quarto(report, path = "report/main.qmd")
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
      tar_quarto(report, path = "report/main.qmd")
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

targets::tar_test("quarto projects", {
  skip_on_cran()
  skip_quarto()
  lines <- c(
    "project:",
    "  type: book",
    "book:",
    "  title: 'r'",
    "  author: 'a'",
    "  chapters:",
    "    - index.qmd",
    "    - r2.qmd"
  )
  writeLines(lines, "_quarto.yml")
  lines <- c(
    "# Preface {.unnumbered}",
    "",
    "r1.",
    "",
    "```{r}",
    "targets::tar_read(r1)",
    "```"
  )
  writeLines(lines, "index.qmd")
  lines <- c(
    "# r2",
    "",
    "r2.",
    "",
    "```{r}",
    "targets::tar_read(r2)",
    "```"
  )
  writeLines(lines, "r2.qmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(r1, "r1_value"),
      tar_target(r2, "r2_value"),
      tar_quarto(project)
    )
  }, ask = FALSE)
  edges <- targets::tar_network(callr_function = NULL)$edges
  expect_equal(sort(edges$from), sort(c("r1", "r2")))
  expect_equal(edges$to, c("project", "project"))
  targets::tar_make(callr_function = NULL)
  expect_true(
    any(grepl("r1_value", readLines("_book/index.html", warn = FALSE)))
  )
  expect_true(any(grepl("r2_value", readLines("_book/r2.html", warn = FALSE))))
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  unlink("_book/search.json")
  expect_equal(targets::tar_outdated(callr_function = NULL), "project")
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(r1, "r1_value"),
      tar_target(r2, "r2_value2"),
      tar_quarto(project)
    )
  }, ask = FALSE)
  expect_equal(
    sort(targets::tar_outdated(callr_function = NULL)),
    sort(c("project", "r2"))
  )
})

targets::tar_test("quarto profiles", {
  skip_on_cran()
  skip_quarto()
  writeLines(
    c(
      "---",
      "title: Testing",
      "---",
      "",
      "Test."
    ),
    "testing.qmd"
  )
  writeLines(
    c("project:",
      "  output-dir: _output",
      "",
      "profile:",
      "  default: basic"
    ),
    "_quarto.yml"
  )
  writeLines(
    c(
      "author: Basic author",
      "",
      "format:",
      "  html:",
      "    output-file: 'basic.html'"
    ),
    "_quarto-basic.yml"
  )
  writeLines(
    c(
      "author: Advanced author",
      "",
      "format:",
      "  html:",
      "    output-file: 'advanced.html'"
    ),
    "_quarto-advanced.yml"
  )
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_quarto(test, path = ".", profile = "basic")
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_true(file.exists(file.path("_output", "basic.html")))
  expect_false(file.exists(file.path("_output", "advanced.html")))
  unlink("_output", recursive = TRUE)
  targets::tar_destroy()
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_quarto(test, path = ".", profile = "advanced")
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_false(file.exists(file.path("_output", "basic.html")))
  expect_true(file.exists(file.path("_output", "advanced.html")))
})

targets::tar_test("tar_quarto() deprecate packages", {
  skip_on_cran()
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
  expect_warning(
    tar_quarto(report, path = "report.qmd", packages = "quarto"),
    class = "tar_condition_deprecate"
  )
})

targets::tar_test("tar_quarto() creates custom output file", {
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
      tar_quarto(report, path = "report.qmd", output_file = "test.html")
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  out <- setdiff(out, "report_files")
  expect_equal(sort(basename(out)), sort(c("test.html", "report.qmd")))
  expect_false(file.exists(file.path("report.html")))
  expect_true(file.exists(file.path("test.html")))
})

targets::tar_test("tar_quarto() reruns if target changes in included file", {
  skip_quarto()
  lines <- c(
    "---",
    "title: main",
    "output_format: html",
    "---",
    "",
    "{{< include \"file1.qmd\" >}}"
  )
  writeLines(lines, "main.qmd")
  lines <- c(
    "# First File",
    "",
    "Contains a code cell with a target.",
    "",
    "```{r}",
    "targets::tar_read(data)",
    "```"
  )
  writeLines(lines, "file1.qmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = seq_len(26L), y = letters)),
      tar_quarto(report, path = "main.qmd")
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("data", "report")))
  out <- targets::tar_read(report)
  out <- setdiff(out, "main_files")
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(
      # On the windows CI, there seems to be issues and `fs::path_rel`. Because
      # of that, `file1.qmd` is returned twice and we use `unique` here. On
      # other platforms this issue does not exist.
      #
      # See https://github.com/ropensci/tarchetypes/pull/200 for a discussion.
      sort(unique(basename(out))),
      sort(c("main.html", "main.qmd", "file1.qmd"))
    )
  } else {
    expect_equal(
      sort(basename(out)),
      sort(c("main.html", "main.qmd", "file1.qmd"))
    )
  }
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  # Should rerun the report.
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_target(data, data.frame(x = rev(seq_len(26L)), y = letters)),
      tar_quarto(report, path = "main.qmd")
    )
  })
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("data", "report")))
})

targets::tar_test("tar_quarto() reruns if an included file changes", {
  skip_quarto()
  lines <- c(
    "---",
    "title: main",
    "output_format: html",
    "---",
    "",
    "{{< include \"file1.qmd\" >}}"
  )
  writeLines(lines, "main.qmd")
  lines <- c(
    "# First File",
    "",
    "Includes another file.",
    "",
    "{{< include \"file2.qmd\" >}}"
  )
  writeLines(lines, "file1.qmd")
  lines <- c(
    "# Second File",
    "",
    "Does not include another file."
  )
  writeLines(lines, "file2.qmd")
  targets::tar_script({
    library(tarchetypes)
    list(
      tar_quarto(report, path = "main.qmd")
    )
  })
  # First run.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(sort(progress$name), sort(c("report")))
  out <- targets::tar_read(report)
  out <- setdiff(out, "main_files")
  expect_equal(
    sort(basename(out)),
    sort(c("main.html", "main.qmd", "file1.qmd", "file2.qmd"))
  )
  # Should not rerun the report.
  suppressMessages(targets::tar_make(callr_function = NULL))
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  # Should rerun the report.
  lines <- c(
    "# Second File",
    "",
    "A change to the second file."
  )
  writeLines(lines, "file2.qmd")
  suppressMessages(targets::tar_make(callr_function = NULL))
  expect_equal(sort(targets::tar_progress()$name), sort(c("report")))
})
