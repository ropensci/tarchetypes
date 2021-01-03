targets::tar_test("tar_skip() can cancel", {
  tar_script({
    list(
      tarchetypes::tar_skip(x, command = "value", skip = 1 > 0)
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_false(file.exists(targets::tar_path(x)))
})

targets::tar_test("tar_skip() does not always cancel", {
  tar_script({
    list(
      tarchetypes::tar_skip(x, command = "value", skip = 1 < 0)
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), "value")
})
