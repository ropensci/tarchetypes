targets::tar_test("tar_sub()", {
  values <- list(
    name = rlang::syms(c("name1", "name2")),
    file = list("file1.Rmd", "file2.Rmd")
  )
  out <- tar_sub(tar_render(name, file), values = values)
  expect_true(is.language(out[[1]]))
  expect_true(is.language(out[[2]]))
  out <- lapply(out, deparse_language)
  expect_equal(out[[1]], "expression(tar_render(name1, \"file1.Rmd\"))")
  expect_equal(out[[2]], "expression(tar_render(name2, \"file2.Rmd\"))")
})
