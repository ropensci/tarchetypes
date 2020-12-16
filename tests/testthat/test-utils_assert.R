targets::tar_test("assert_equal_lengths()", {
  expect_silent(assert_equal_lengths(letters))
  x <- list(x = seq_len(2), y = seq_len(3))
  expect_error(assert_equal_lengths(x), class = "condition_validate")
})

targets::tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "condition_validate")
})

targets::tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

targets::tar_test("assert_chr()", {
  expect_silent(assert_chr(letters))
  expect_error(assert_chr(123), class = "condition_validate")
})

targets::tar_test("assert_dbl()", {
  expect_silent(assert_dbl(1.2))
  expect_error(assert_dbl(letters), class = "condition_validate")
})

targets::tar_test("assert_inherits()", {
  expect_silent(assert_inherits(structure(list(), class = "xyz"), "xyz"))
  expect_error(assert_inherits(letters, "xyz"), class = "condition_validate")
})

targets::tar_test("assert_int()", {
  expect_silent(assert_int(123L))
  expect_error(assert_int(letters), class = "condition_validate")
})

targets::tar_test("assert_lang()", {
  expect_silent(assert_lang(as.symbol("abc")))
  expect_silent(assert_lang(quote(1 + 1)))
  expect_error(assert_lang("abc"), class = "condition_validate")
})

targets::tar_test("assert_list()", {
  expect_silent(assert_list(list("abc")))
  expect_error(assert_list("abc"), class = "condition_validate")
})

targets::tar_test("assert_names()", {
  expect_silent(assert_names(c("a", "b")))
  expect_error(assert_names(c("a", "_b")), class = "condition_validate")
})

targets::tar_test("assert_nonempty()", {
  expect_silent(assert_nonempty(c("a", "b")))
  expect_error(assert_nonempty(list()), class = "condition_validate")
})

targets::tar_test("assert_not_expr()", {
  expect_silent(assert_not_expr(quote(x)))
  expect_error(assert_not_expr(expression(x)), class = "condition_validate")
})

targets::tar_test("assert_not_in()", {
  expect_silent(assert_not_in("x", c("a", "b")))
  expect_error(assert_not_in("b", c("a", "b")), class = "condition_validate")
})

targets::tar_test("assert_nzchr()", {
  expect_silent(assert_nzchr(c("a", "b")))
  expect_error(assert_nzchr(c("a", "")), class = "condition_validate")
})

targets::tar_test("assert_identical()", {
  expect_silent(assert_identical("a", "a"))
  expect_error(assert_identical(1L, 2L), class = "condition_validate")
})

targets::tar_test("assert_identical_chr()", {
  expect_silent(assert_identical_chr("a", "a"))
  expect_error(assert_identical_chr("1", "2"), class = "condition_validate")
})

targets::tar_test("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(letters), class = "condition_validate")
})

targets::tar_test("assert_envir()", {
  expect_silent(assert_envir(emptyenv()))
  expect_error(assert_envir(letters), class = "condition_validate")
})

targets::tar_test("assert_unique()", {
  expect_silent(assert_unique(letters))
  expect_error(assert_unique(c("1", "1")), class = "condition_validate")
})
