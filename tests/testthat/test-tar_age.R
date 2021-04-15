targets::tar_test("tar_age() with short age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      tarchetypes::tar_age(
        data,
        0L,
        age = as.difftime(0.0001, units = "secs")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(data), 0L)
  Sys.sleep(0.25)
  targets::tar_make(callr_function = NULL)
  expect_equal(tar_progress()$name, "data")
})

targets::tar_test("tar_age() with long age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      tarchetypes::tar_age(
        data,
        0L,
        age = as.difftime(9999, units = "weeks")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(data), 0L)
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
})

targets::tar_test("tar_age() with dynamic branching and short age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(x, seq_len(2L)),
      tarchetypes::tar_age(
        y,
        x,
        pattern = map(x),
        age = as.difftime(0.0001, units = "secs")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(y), seq_len(2L))
  Sys.sleep(0.25)
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(targets::tar_progress()), 3L)
  expect_true("y" %in% targets::tar_progress()$name)
  expect_true(
    is.environment(tar_age(x, 1, age = as.difftime(1, units = "secs")))
  )
})

targets::tar_test("tar_age() with dynamic branching and long age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(x, seq_len(2L)),
      tarchetypes::tar_age(
        y,
        x,
        pattern = map(x),
        age = as.difftime(9999, units = "weeks")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(y), seq_len(2L))
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(targets::tar_progress()), 0L)
  expect_true(
    is.environment(tar_age(x, 1, age = as.difftime(1, units = "secs")))
  )
})
