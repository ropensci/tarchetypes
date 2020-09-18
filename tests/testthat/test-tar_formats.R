tar_test("tar_file()", {
  x <- tar_file(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "file")
})

tar_test("tar_url()", {
  x <- tar_url(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "url")
})

tar_test("tar_rds()", {
  x <- tar_rds(x, 0)
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$format, "rds")
})

tar_test("tar_rds() runs", {
  targets::tar_script(targets::tar_pipeline(tarchetypes::tar_rds(x, 0L)))
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), 0L)
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

tar_test("tar_rds() with pattern", {
  x <- tar_rds(x, 1, pattern = map(y))
  expect_true(inherits(x, "tar_target"))
  expect_equal(x$settings$dimensions, "y")
})
