targets::tar_test("tar_change() produces the expected commands", {
  targets::tar_script({
    list(
      tarchetypes::tar_change(x, command = value, change = tempfile())
    )
  })
  out_x <- targets::tar_manifest(x, callr_function = NULL)
  out_change <- targets::tar_manifest(x_change, callr_function = NULL)
  expect_equal(out_x$name, "x")
  expect_true(grepl("x_change", out_x$command))
  expect_true(grepl("value", out_x$command))
  expect_equal(out_change$name, "x_change")
  expect_equal(out_change$command, "tempfile()")
})

targets::tar_test("tar_change() creates a target that responds to change", {
  targets::tar_script({
    list(
      tarchetypes::tar_change(x, paste0("1", tempfile()), change = tempfile())
    )
  })
  targets::tar_make(callr_function = NULL)
  out_x <- targets::tar_read(x)
  out_change <- targets::tar_read(x_change)
  expect_true(grepl("^1", out_x))
  expect_false(grepl("^1", out_change))
  targets::tar_make(callr_function = NULL)
  expect_false(out_x == targets::tar_read(x))
  expect_false(out_change == targets::tar_read(x_change))
})

targets::tar_test("tar_change() with tidy eval", {
  targets::tar_script({
    x <- "val_x"
    y <- "val_y"
    list(
      tarchetypes::tar_change(x, !!x, change = !!y)
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_true(any(grepl("val_x", out$command)))
  expect_true(any(grepl("val_y", out$command)))
  targets::tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "val_x")
  expect_equal(tar_read(x_change), "val_y")
})

targets::tar_test("tar_change() without tidy eval", {
  targets::tar_script({
    x <- "val_x"
    y <- "val_y"
    list(
      tarchetypes::tar_change(x, !!x, change = !!y, tidy_eval = FALSE)
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_true(any(grepl("!!x", out$command)))
  expect_true(any(grepl("!!y", out$command)))
})
