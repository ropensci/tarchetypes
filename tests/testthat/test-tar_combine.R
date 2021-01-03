targets::tar_test("tar_combine() with defaults", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  out <- tar_combine(
    new_target_name,
    target1,
    target2
  )
  expect_equal(out$settings$name, "new_target_name")
  expect_equal(out$command$string, "expression(vctrs::vec_c(x = x, y = y))")
})

targets::tar_test("tar_combine() with no names", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  out <- tar_combine(
    new_target_name,
    target1,
    target2,
    use_names = FALSE
  )
  expect_equal(out$settings$name, "new_target_name")
  expect_equal(out$command$string, "expression(vctrs::vec_c(x, y))")
})

targets::tar_test("tar_combine() with custom command", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  target3 <- tar_combine(
    new_target_name,
    target1,
    target2,
    command = bind_rows(!!!.x)
  )
  expect_equal(target3$command$string, "expression(bind_rows(x = x, y = y))")
})

targets::tar_test("tar_combine() run", {
  tar_script({
    target1 <- targets::tar_target(x, 1)
    target2 <- targets::tar_target(y, 2)
    target3 <- tarchetypes::tar_combine(
      new_target_name,
      target1,
      target2,
      use_names = FALSE
    )
    list(target1, target2, target3)
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(new_target_name), seq_len(2))
})

targets::tar_test("tar_combine_raw() with defaults", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  out <- tar_combine_raw(
    "new_target_name",
    target1,
    target2
  )
  expect_equal(out$settings$name, "new_target_name")
  expect_equal(out$command$string, "expression(vctrs::vec_c(x = x, y = y))")
})

targets::tar_test("tar_combine_raw() with no names", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  out <- tar_combine_raw(
    "new_target_name",
    target1,
    target2,
    use_names = FALSE
  )
  expect_equal(out$settings$name, "new_target_name")
  expect_equal(out$command$string, "expression(vctrs::vec_c(x, y))")
})

targets::tar_test("tar_combine_raw() with custom command", {
  target1 <- targets::tar_target(x, head(mtcars))
  target2 <- targets::tar_target(y, tail(mtcars))
  target3 <- tar_combine_raw(
    "new_target_name",
    target1,
    target2,
    command = expression(bind_rows(!!!.x))
  )
  expect_equal(target3$command$string, "expression(bind_rows(x = x, y = y))")
})

targets::tar_test("tar_combine_raw() run", {
  tar_script({
    target1 <- targets::tar_target(x, 1)
    target2 <- targets::tar_target(y, 2)
    target3 <- tarchetypes::tar_combine_raw(
      "new_target_name",
      target1,
      target2,
      use_names = FALSE
    )
    list(target1, target2, target3)
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(new_target_name), seq_len(2))
})
