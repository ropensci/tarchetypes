targets::tar_test("tar_plan() works", {
  targets::tar_script({
    tarchetypes::tar_plan(
      x = 1,
      y = 2,
      tar_target(z, 3)
    )
  })
  out <- tar_manifest(x, callr_function = NULL)
  expect_equal(out$command, "1")
  out <- tar_manifest(y, callr_function = NULL)
  expect_equal(out$command, "2")
  out <- tar_manifest(z, callr_function = NULL)
  expect_equal(out$command, "3")
  targets::tar_make(callr_function = NULL)
  expect_equal(tar_read(x), 1)
  expect_equal(tar_read(y), 2)
  expect_equal(tar_read(z), 3)
})

targets::tar_test("tar_plan() still allows tidy eval", {
  targets::tar_script({
    y_val <- 4
    z_val <- 5
    tarchetypes::tar_plan(
      x = 1,
      y = !!y_val,
      tar_target(z, !!z_val),
      tar_target(w, !!w_val, tidy_eval = FALSE)
    )
  })
  out <- tar_manifest(x, callr_function = NULL)
  expect_equal(out$command, "1")
  out <- tar_manifest(y, callr_function = NULL)
  expect_equal(out$command, "4")
  out <- tar_manifest(z, callr_function = NULL)
  expect_equal(out$command, "5")
  out <- tar_manifest(w, callr_function = NULL)
  expect_equal(out$command, "!!w_val")
})

targets::tar_test("tar_plan() allows trailing commas", {
  out <- tar_plan(a = 1, )
  expect_equal(length(out), 1L)
  expect_true(inherits(out[[1]], "tar_target"))
})
