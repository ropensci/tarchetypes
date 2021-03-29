targets::tar_test("knitr_deps() on a bad report", {
  writeLines(c("```{r}", "1 <-", "```"), "report.Rmd")
  expect_error(knitr_deps("report.Rmd"), class = "tar_condition_validate")
})

targets::tar_test("knitr_deps()", {
  path <- system.file("example_rmd.Rmd", package = "tarchetypes")
  expect_equal(
    sort(knitr_deps(path)),
    sort(c("analysis", "data", "data2", "string1", "string2", "string3"))
  )
})

targets::tar_test("walk_call_knitr() finds function dependencies", {
  f <- function() tar_load(x)
  expect_equal(walk_ast(f, walk_call_knitr), "x")
})

targets::tar_test("walk_call_knitr() warns on empty tar_load()", {
  expr <- quote(tar_load())
  expect_warning(
    out <- walk_ast(expr, walk_call_knitr),
    class = "tar_condition_validate"
  )
  expect_equal(out, character(0))
})

targets::tar_test("walk_call_knitr() warns on empty tar_read()", {
  expr <- quote(tar_read())
  expect_warning(
    out <- walk_ast(expr, walk_call_knitr),
    class = "tar_condition_validate"
  )
  expect_equal(out, character(0))
})

targets::tar_test("walk_call_knitr() warns on tidyselect", {
  expr <- quote(tar_load(starts_with("xyz")))
  expect_warning(
    out <- walk_ast(expr, walk_call_knitr),
    class = "tar_condition_validate"
  )
  expect_equal(out, character(0))
})
