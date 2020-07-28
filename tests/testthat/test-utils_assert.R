tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "condition_validate")
})

tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

tar_test("assert_chr()", {
  expect_silent(assert_chr(letters))
  expect_error(assert_chr(123), class = "condition_validate")
})

tar_test("assert_int()", {
  expect_silent(assert_int(123L))
  expect_error(assert_int(letters), class = "condition_validate")
})

tar_test("assert_identical()", {
  expect_silent(assert_identical("a", "a"))
  expect_error(assert_identical(1L, 2L), class = "condition_validate")
})

tar_test("assert_identical_chr()", {
  expect_silent(assert_identical_chr("a", "a"))
  expect_error(assert_identical_chr("1", "2"), class = "condition_validate")
})

tar_test("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(letters), class = "condition_validate")
})

tar_test("assert_envir()", {
  expect_silent(assert_envir(emptyenv()))
  expect_error(assert_envir(letters), class = "condition_validate")
})

tar_test("assert_unique()", {
  expect_silent(assert_unique(letters))
  expect_error(assert_unique(c("1", "1")), class = "condition_validate")
})
