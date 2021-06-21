targets::tar_test("assert_hook_placeholder", {
  expect_silent(assert_hook_placeholder(quote(f(.x))))
  expect_silent(assert_hook_placeholder(expression(f(.x))))
  expect_error(
    assert_hook_placeholder(quote(f())),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_hook_expr", {
  expect_silent(assert_hook_expr(tar_target(x, f())))
  expect_silent(assert_hook_expr(tar_target(x, NULL)))
  x <- tar_target(y, 1)
  x$command$expr <- 123
  expect_error(
    assert_hook_expr(x),
    class = "tar_condition_validate"
  )
  x$command$expr <- expression(z <- 1, z)
  expect_error(
    assert_hook_expr(x),
    class = "tar_condition_validate"
  )
})
