targets::tar_test("tar_quarto_files() single Rmd/qmd", {
  skip_quarto()
  lines <- c(
    "---",
    "title: source file",
    "---",
    "Assume these lines are in report.qmd.",
    "```{r}",
    "1 + 1",
    "```"
  )
  for (ext in c(".qmd", ".Rmd")) {
    fs::dir_create("x")
    path <- file.path("x", paste0("report", ext))
    writeLines(lines, path)
    out <- tar_quarto_files(path)
    expect_equal(out$sources, character(0))
    expect_equal(out$output, file.path("x", "report.html"))
    expect_equal(out$input, file.path("x", paste0("report", ext)))
  }
})

targets::tar_test("tar_quarto_files() project", {
  skip_on_cran()
  skip_quarto()
  fs::dir_create("x")
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
  writeLines(lines, file.path("x", "_quarto.yml"))
  lines <- c(
    "# Preface {.unnumbered}",
    "",
    "r1.",
    "",
    "```{r}",
    "targets::tar_read(r1)",
    "```"
  )
  writeLines(lines, file.path("x", "index.qmd"))
  lines <- c(
    "# r2",
    "",
    "r2.",
    "",
    "```{r}",
    "targets::tar_read(r2)",
    "```"
  )
  writeLines(lines, file.path("x", "r2.qmd"))
  info <- tar_quarto_files("x")
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(
      sort(basename(info$sources)),
      sort(c("index.qmd", "r2.qmd"))
    )
    expect_equal(basename(info$output), "_book")
    expect_equal(
      sort(basename(info$input)),
      sort(c("_quarto.yml", "index.qmd", "r2.qmd"))
    )
  } else {
    expect_equal(
      sort(info$sources),
      sort(file.path("x", c("index.qmd", "r2.qmd")))
    )
    expect_equal(info$output, file.path("x", "_book"))
    expect_equal(
      sort(info$input),
      sort(file.path("x", c("_quarto.yml", "index.qmd", "r2.qmd")))
    )
  }
})

targets::tar_test("tar_quarto_files() detects non-code dependencies", {
  skip_quarto()
  fs::dir_create("report")
  fs::dir_create(file.path("report", "subdir"))
  lines <- c(
    "---",
    "title: main",
    "output_format: html",
    "---",
    "",
    "{{< include \"text1.qmd\" >}}"
  )
  writeLines(lines, file.path("report", "main.qmd"))
  lines <- c(
    "# First File",
    "",
    "Some text here.",
    "",
    "{{< include \"subdir/text2.qmd\" >}}"
  )
  writeLines(lines, file.path("report", "text1.qmd"))
  lines <- c(
    "# Second File",
    "",
    "Some text here."
  )
  writeLines(lines, file.path("report", "subdir", "text2.qmd"))
  out <- tar_quarto_files(file.path("report", "main.qmd"))
  expect_equal(out$sources, character(0))
  expect_equal(out$output, file.path("report", "main.html"))
  expect_equal(
    sort(out$input),
    sort(
      c(
        file.path("report", c("main.qmd", "text1.qmd")),
        file.path("report", "subdir", "text2.qmd")
      )
    )
  )
  # Check whether we get the same result with a reference to the directory
  # instead of the file.
  lines <- c(
    "project:",
    "  output-dir: myoutdir",
    "  render:",
    "    - main.qmd"
  )
  writeLines(lines, file.path("report", "_quarto.yml"))
  out <- tar_quarto_files("report/")
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(basename(out$sources), character(0))
    expect_equal(basename(out$output), "myoutdir")
    expect_equal(
      sort(basename(out$input)),
      sort(c("_quarto.yml", "main.qmd", "text1.qmd", "text2.qmd"))
    )
  } else {
    expect_equal(out$sources, character(0))
    expect_equal(out$output, file.path("report", "myoutdir"))
    expect_equal(
      sort(out$input),
      sort(
        c(
          file.path(
            "report",
            c("_quarto.yml", "main.qmd", "text1.qmd")
          ),
          file.path("report", "subdir", "text2.qmd")
        )
      )
    )
  }
})

targets::tar_test("tar_quarto_files() detects custom output file", {
  skip_quarto()
  lines <- c(
    "---",
    "title: source file",
    "format:",
    "  html:",
    "    output-file: custom.html",
    "---",
    "Assume these lines are in report.qmd.",
    "```{r}",
    "1 + 1",
    "```"
  )
  writeLines(lines, "report.qmd")
  out <- tar_quarto_files("report.qmd")
  expect_equal(out$sources, character(0))
  expect_equal(out$output, "custom.html")
  expect_equal(out$input, "report.qmd")
})

targets::tar_test("tar_quarto_files() detects code dependencies", {
  skip_quarto()
  fs::dir_create("report")
  fs::dir_create(file.path("report", "subdir"))
  lines <- c(
    "---",
    "title: main",
    "output_format: html",
    "---",
    "",
    "{{< include \"text1.qmd\" >}}",
    "",
    "{{< include \"subdir/text2.qmd\" >}}"
  )
  writeLines(lines, file.path("report", "main.qmd"))
  lines <- c(
    "# First File",
    "",
    "Contains a code cell.",
    "",
    "```{r}",
    "1 + 1",
    "```"
  )
  writeLines(lines, file.path("report", "text1.qmd"))
  lines <- c(
    "# Second File",
    "",
    "Contains a code cell with a call to `tar_read`.",
    "",
    "```{r}",
    "targets::tar_read(mytarget)",
    "```"
  )
  writeLines(lines, file.path("report", "subdir", "text2.qmd"))
  out <- tar_quarto_files("report/main.qmd")
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(basename(out$sources), "text2.qmd")
    expect_equal(basename(out$output), "main.html")
    expect_equal(
      sort(basename(out$input)),
      sort(c("main.qmd", "text1.qmd", "text2.qmd"))
    )
  } else {
    expect_equal(out$sources, file.path("report", "subdir", "text2.qmd"))
    expect_equal(out$output, file.path("report", "main.html"))
    expect_equal(
      sort(out$input),
      sort(
        c(
          file.path("report", c("main.qmd", "text1.qmd")),
          file.path("report", "subdir", "text2.qmd")
        )
      )
    )
  }
  # Check whether we get the same result when calling via a project.
  lines <- c(
    "project:",
    "  output-dir: myoutdir",
    "  render:",
    "    - main.qmd"
  )
  writeLines(lines, file.path("report", "_quarto.yml"))
  out <- tar_quarto_files("report/")
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    expect_equal(basename(out$sources), "text2.qmd")
    expect_equal(basename(out$output), "myoutdir")
    expect_equal(
      sort(basename(out$input)),
      sort(c("_quarto.yml", "main.qmd", "text1.qmd", "text2.qmd"))
    )
  } else {
    expect_equal(out$sources, file.path("report", "subdir", "text2.qmd"))
    expect_equal(out$output, file.path("report", "myoutdir"))
    expect_equal(
      sort(out$input),
      sort(
        c(
          file.path(
            "report",
            c("_quarto.yml", "main.qmd", "text1.qmd")
          ),
          file.path("report", "subdir", "text2.qmd")
        )
      )
    )
  }
})
