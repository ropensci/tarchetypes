targets::tar_test("tar_eval()", {
  values <- list(
    value1 = c(1L, 2L, 3L),
    value2 = c(4L, 5L, 6L)
  )
  out <- unlist(tar_eval(value1 + value2, values = values))
  expect_equal(out, c(5L, 7L, 9L))
})
