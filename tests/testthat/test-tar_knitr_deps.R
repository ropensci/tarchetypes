targets::tar_test("tar_knitr_deps()", {
  skip_pandoc()
  lines1 <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "tar_load(data1)",
    "tar_read(data2)",
    "```"
  )
  lines2 <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "tar_load(data2)",
    "tar_read(data3)",
    "```"
  )
  report1 <- tempfile()
  report2 <- tempfile()
  writeLines(lines1, report1)
  writeLines(lines2, report2)
  out <- tar_knitr_deps(c(report1, report2))
  exp <- c("data1", "data2", "data3")
  expect_equal(sort(out), sort(exp))
})
