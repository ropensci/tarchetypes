targets::tar_test("deparse_safe() on long expression", {
  out <- deparse_safe(letters)
  expect_equal(length(out), 1L)
  out <- eval(parse(text = out))
  expect_equal(out, letters)
})
