targets::tar_test("tar_skip() can cancel", {
  tar_script({
    list(
      tarchetypes::tar_skip(x, command = "value", skip = 1 > 0)
    )
  })
  targets::tar_make(callr_function = NULL)
  path <- file.path(targets::tar_config_get("store"), "objects", "x")
  expect_false(file.exists(file.path(path)))
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
