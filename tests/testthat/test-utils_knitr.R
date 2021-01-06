targets::tar_test("knitr_deps() on a bad report", {
  writeLines(c("```{r}", "1 <-", "```"), "report.Rmd")
  expect_error(knitr_deps("report.Rmd"), class = "condition_validate")
})

targets::tar_test("knitr_deps()", {
  path <- system.file("example_rmd.Rmd", package = "tarchetypes")
  file.copy(path, "report.Rmd")
  expect_equal(
    sort(knitr_deps(path)),
    sort(c("analysis", "data", "data2", "string1", "string2", "string3"))
  )
})

targets::tar_test("walk_expr() finds function dependencies", {
  f <- function() tar_load(x)
  counter <- counter_init()
  walk_expr(f, counter)
  expect_equal(counter_get_names(counter), "x")
})

targets::tar_test("walk_expr() warns on empty tar_load()", {
  expr <- quote(tar_load())
  counter <- counter_init()
  expect_warning(walk_expr(expr, counter), class = "condition_validate")
  expect_equal(counter_get_names(counter), character(0))
})

targets::tar_test("walk_expr() warns on empty tar_read()", {
  expr <- quote(tar_read())
  counter <- counter_init()
  expect_warning(walk_expr(expr, counter), class = "condition_validate")
  expect_equal(counter_get_names(counter), character(0))
})

targets::tar_test("walk_expr() warns on tidyselect", {
  expr <- quote(tar_load(starts_with("xyz")))
  counter <- counter_init()
  expect_warning(walk_expr(expr, counter), class = "condition_validate")
  expect_equal(counter_get_names(counter), character(0))
})
