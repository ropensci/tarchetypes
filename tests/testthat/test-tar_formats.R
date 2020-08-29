tar_test("tar_file()", {
  x <- tar_file(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "file")
})

tar_test("tar_rds()", {
  x <- tar_rds(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "rds")
})

tar_test("tar_qs()", {
  x <- tar_qs(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "qs")
})

tar_test("tar_fst()", {
  x <- tar_fst(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "fst")
})

tar_test("tar_fst_dt()", {
  x <- tar_fst_dt(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "fst_dt")
})

tar_test("tar_fst_tbl()", {
  x <- tar_fst_tbl(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "fst_tbl")
})

tar_test("tar_keras()", {
  x <- tar_keras(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "keras")
})

tar_test("tar_format_api()", {
  expect_null(tar_format_api(x, 1))
})

tar_test("tar_format()", {
  f <- tar_format("file")
  x <- f(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "file")
})
