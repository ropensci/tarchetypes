tar_test("tar_map() return value", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78))
  )
  expect_equal(length(out), 4)
  map(out, ~expect_true(inherits(.x, "tar_target")))
})

tar_test("tar_map() with names turned off", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    names = NULL
  )
  names <- map_chr(out, ~.x$settings$name)
  expect_equal(sort(names), sort(c("x_1", "x_2", "y_1", "y_2")))
})

tar_test("tar_map() with names misspecified", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    names = "z"
  )
  names <- map_chr(out, ~.x$settings$name)
  expect_equal(sort(names), sort(c("x_1", "x_2", "y_1", "y_2")))
})

tar_test("tar_map() manifest", {
  targets::tar_script({
    targets::tar_pipeline(
      tarchetypes::tar_map(
        targets::tar_target(x, a + b),
        targets::tar_target(y, x + a, pattern = map(x)),
        values = list(a = c(12, 34), b = c(56, 78))
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(dim(out), c(4, 4))
  names <- c("x_12_56", "x_34_78", "y_12_56", "y_34_78")
  expect_equal(sort(out$name), sort(c(names)))
  expect_equal(out$command[out$name == "x_12_56"], "12 + 56")
  expect_equal(out$command[out$name == "x_34_78"], "34 + 78")
  expect_equal(out$command[out$name == "y_12_56"], "x_12_56 + 12")
  expect_equal(out$command[out$name == "y_34_78"], "x_34_78 + 34")
  expect_equal(out$type[out$name == "x_12_56"], "stem")
  expect_equal(out$type[out$name == "x_34_78"], "stem")
  expect_equal(out$type[out$name == "y_12_56"], "map")
  expect_equal(out$type[out$name == "y_34_78"], "map")
  expect_equal(out$dimensions[out$name == "x_12_56"][[1]], character(0))
  expect_equal(out$dimensions[out$name == "x_34_78"][[1]], character(0))
  expect_equal(out$dimensions[out$name == "y_12_56"][[1]], "x_12_56")
  expect_equal(out$dimensions[out$name == "y_34_78"][[1]], "x_34_78")
})

tar_test("tar_map() values", {
  targets::tar_script({
    targets::tar_pipeline(
      tarchetypes::tar_map(
        targets::tar_target(x, a + b),
        targets::tar_target(y, x + a, pattern = map(x)),
        values = list(a = c(12, 34), b = c(56, 78))
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(tar_read(x_12_56), 68)
  expect_equal(tar_read(x_34_78), 112)
  expect_equal(tar_read(y_12_56), 80)
  expect_equal(tar_read(y_34_78), 146)
})
