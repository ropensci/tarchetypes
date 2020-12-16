targets::tar_test("counter_get_names()", {
  out <- counter_get_names(counter_init(letters))
  expect_equal(sort(out), sort(letters))
})

targets::tar_test("counter$exists_name()", {
  out <- counter_init(letters)
  expect_true(counter_exists_name(out, "b"))
  expect_false(counter_exists_name(out, "abcde"))
})

targets::tar_test("counter_set_name(new)", {
  x <- counter_init(letters)
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 26L)
  counter_set_name(x, "abc")
  expect_true(counter_exists_name(x, "abc"))
  expect_equal(x$count, 27L)
})

targets::tar_test("counter_set_name(new) on an empty counter", {
  x <- counter_init()
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 0L)
  counter_set_name(x, "abc")
  expect_true(counter_exists_name(x, "abc"))
  expect_equal(x$count, 1L)
})

targets::tar_test("counter_set_name(old)", {
  x <- counter_init(letters)
  expect_true(counter_exists_name(x, "b"))
  expect_equal(x$count, 26L)
  counter_set_name(x, "b")
  expect_true(counter_exists_name(x, "b"))
  expect_equal(x$count, 26L)
})

targets::tar_test("counter_validate() nonempty counter", {
  x <- counter_init(letters)
  expect_silent(counter_validate(x))
})

targets::tar_test("counter_validate() empty counter", {
  x <- counter_init()
  expect_silent(counter_validate(x))
})

targets::tar_test("counter_validate() with a count mismatch", {
  x <- counter_init(letters)
  x$count <- 0L
  expect_error(counter_validate(x), class = "condition_validate")
})
