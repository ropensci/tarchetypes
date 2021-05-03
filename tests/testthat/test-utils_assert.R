targets::tar_test("assert_equal_lengths()", {
  expect_silent(assert_equal_lengths(letters))
  x <- list(x = seq_len(2), y = seq_len(3))
  expect_error(assert_equal_lengths(x), class = "tar_condition_validate")
})

targets::tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "tar_condition_validate")
})

targets::tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "tar_condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

targets::tar_test("assert_chr()", {
  expect_silent(assert_chr(letters))
  expect_error(assert_chr(123), class = "tar_condition_validate")
})

targets::tar_test("assert_dbl()", {
  expect_silent(assert_dbl(1.2))
  expect_error(assert_dbl(letters), class = "tar_condition_validate")
})

targets::tar_test("assert_df()", {
  expect_silent(assert_df(data.frame(x = 1)))
  expect_error(assert_df(TRUE), class = "tar_condition_validate")
})

targets::tar_test("assert_envir()", {
  expect_silent(assert_envir(emptyenv()))
  expect_error(assert_envir(letters), class = "tar_condition_validate")
})

targets::tar_test("assert_expr", {
  expect_silent(assert_expr(expression("x")))
  expect_error(assert_expr(quote(x + x)), class = "tar_condition_validate")
})

targets::tar_test("assert_ge()", {
  expect_silent(assert_ge(2L, 1L))
  expect_silent(assert_ge(2L, 2L))
  expect_error(assert_ge(1L, 2L), class = "tar_condition_validate")
})

targets::tar_test("assert_hook_placeholder", {
  expect_silent(assert_hook_placeholder(quote(f(.x))))
  expect_silent(assert_hook_placeholder(expression(f(.x))))
  expect_error(
    assert_hook_placeholder(quote(f())),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_hook_expr", {
  expect_silent(assert_hook_expr(tar_target(x, f())))
  expect_silent(assert_hook_expr(tar_target(x, NULL)))
  x <- tar_target(y, 1)
  x$command$expr <- 123
  expect_error(
    assert_hook_expr(x),
    class = "tar_condition_validate"
  )
  x$command$expr <- expression(z <- 1, z)
  expect_error(
    assert_hook_expr(x),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_in()", {
  expect_silent(assert_in("x", letters))
  expect_error(assert_in("xyz", letters), class = "tar_condition_validate")
})

targets::tar_test("assert_inherits()", {
  expect_silent(assert_inherits(structure(list(), class = "xyz"), "xyz"))
  expect_error(
    assert_inherits(letters, "xyz"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_int()", {
  expect_silent(assert_int(123L))
  expect_error(assert_int(letters), class = "tar_condition_validate")
})

targets::tar_test("assert_lang()", {
  expect_silent(assert_lang(as.symbol("abc")))
  expect_silent(assert_lang(quote(1 + 1)))
  expect_error(assert_lang("abc"), class = "tar_condition_validate")
})

targets::tar_test("assert_lgl()", {
  expect_silent(assert_lgl(TRUE))
  expect_error(assert_lgl("abc"), class = "tar_condition_validate")
})

targets::tar_test("assert_list()", {
  expect_silent(assert_list(list("abc")))
  expect_error(assert_list("abc"), class = "tar_condition_validate")
})

targets::tar_test("assert_names()", {
  expect_silent(assert_names(c("a", "b")))
  expect_error(assert_names(c("a", "_b")), class = "tar_condition_validate")
})

targets::tar_test("assert_nonempty()", {
  expect_silent(assert_nonempty(c("a", "b")))
  expect_error(assert_nonempty(list()), class = "tar_condition_validate")
})

targets::tar_test("assert_nonmissing()", {
  expect_silent(assert_nonmissing("abc"))
  expect_error(
    assert_nonmissing(substitute()),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_not_dirs()", {
  expect_silent(assert_not_dirs("x"))
  expect_error(assert_not_dirs(tempdir()), class = "tar_condition_validate")
})

targets::tar_test("assert_not_expr()", {
  expect_silent(assert_not_expr(quote(x)))
  expect_error(
    assert_not_expr(expression(x)),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_not_in()", {
  expect_silent(assert_not_in("x", c("a", "b")))
  expect_error(
    assert_not_in("b", c("a", "b")),
    class = "tar_condition_validate"
  )
})

targets::tar_test("assert_nzchar()", {
  expect_silent(assert_nzchar(c("a", "b")))
  expect_error(assert_nzchar(c("a", "")), class = "tar_condition_validate")
})

targets::tar_test("assert_identical()", {
  expect_silent(assert_identical("a", "a"))
  expect_error(assert_identical(1L, 2L), class = "tar_condition_validate")
})

targets::tar_test("assert_identical_chr()", {
  expect_silent(assert_identical_chr("a", "a"))
  expect_error(assert_identical_chr("1", "2"), class = "tar_condition_validate")
})

targets::tar_test("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(letters), class = "tar_condition_validate")
})

targets::tar_test("assert_unique()", {
  expect_silent(assert_unique(letters))
  expect_error(assert_unique(c("1", "1")), class = "tar_condition_validate")
})
