targets::tar_test("tar_sub()", {
  values <- list(
    name = rlang::syms(c("name1", "name2")),
    file = list("file1.Rmd", "file2.Rmd")
  )
  out <- tar_sub(f(name, file), values = values)
  expect_true(is.language(out[[1]]))
  expect_true(is.language(out[[2]]))
  out <- lapply(out, deparse_language)
  expect_equal(out[[1]], "expression(f(name1, \"file1.Rmd\"))")
  expect_equal(out[[2]], "expression(f(name2, \"file2.Rmd\"))")
})
