tar_test("tar_rds()", {
  x <- tar_rds(x, 1)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "rds")
})
