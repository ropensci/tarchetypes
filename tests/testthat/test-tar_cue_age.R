targets::tar_test("tar_cue_age() with short age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(
        data,
        0L,
        cue = tarchetypes::tar_cue_age(
          name = data,
          age = as.difftime(0.0001, units = "secs")
        )
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(data), 0L)
  Sys.sleep(0.25)
  targets::tar_make(callr_function = NULL)
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(progress$name, "data")
})

targets::tar_test("tar_cue_age() with long age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(
        data,
        0L,
        cue = tarchetypes::tar_cue_age(
          name = data,
          age = as.difftime(9999, units = "weeks")
        )
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(data), 0L)
  targets::tar_make(callr_function = NULL)
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
})

targets::tar_test("tar_cue_age() with dynamic branching and short age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(x, seq_len(2L)),
      targets::tar_target(
        y,
        x,
        pattern = map(x),
        cue = tarchetypes::tar_cue_age(
          name = y,
          age = as.difftime(0.0001, units = "secs")
        )
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(unname(targets::tar_read(y)), seq_len(2L))
  Sys.sleep(0.25)
  targets::tar_make(callr_function = NULL)
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 3L)
  expect_true("y" %in% progress$name)
  expect_true(is.environment(tar_cue_age(x, as.difftime(1, units = "secs"))))
})

targets::tar_test("tar_cue_age() with dynamic branching and long age", {
  targets::tar_script({
    library(tarchetypes)
    list(
      targets::tar_target(x, seq_len(2L)),
      targets::tar_target(
        y,
        x,
        pattern = map(x),
        cue = tarchetypes::tar_cue_age(
          name = y,
          age = as.difftime(9999, units = "weeks")
        )
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(unname(targets::tar_read(y)), seq_len(2L))
  targets::tar_make(callr_function = NULL)
  progress <- targets::tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  expect_true(is.environment(tar_cue_age(x, as.difftime(1, units = "secs"))))
})
