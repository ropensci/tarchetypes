targets::tar_test("knitr_deps() on a bad report", {
  writeLines(c("```{r}", "1 <-", "```"), "report.Rmd")
  expect_error(knitr_deps("report.Rmd"), class = "condition_validate")
})

targets::tar_test("knitr_deps()", {
  path <- system.file("example_rmd.Rmd", package = "tarchetypes")
  file.copy(path, "report.Rmd")
  expect_equal(sort(knitr_deps(path)), sort(c("analysis", "data", "data2")))
})

targets::tar_test("walk_expr() finds function dependencies", {
  f <- function() tar_load(x)
  counter <- counter_init()
  walk_expr(f, counter)
  expect_equal(counter_get_names(counter), "x")
})
