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
    expect_equal(out$sources, file.path("x", paste0("report", ext)))
    expect_equal(out$output, file.path("x", "report.html"))
    expect_equal(out$input, character(0))
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
    expect_equal(basename(info$input), "_quarto.yml")
  } else {
    expect_equal(
      sort(info$sources),
      sort(file.path("x", c("index.qmd", "r2.qmd")))
    )
    expect_equal(info$output, file.path("x", "_book"))
    expect_equal(info$input, file.path("x", "_quarto.yml"))
  }
})
