targets::tar_test("tar_cue_force(FALSE)", {
  targets::tar_script(
    targets::tar_target(x, 0L, cue = tar_cue_force(0 > 1))
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), 0L)
  for (i in seq_len(2)) {
    targets::tar_make(callr_function = NULL)
    progress <- targets::tar_progress()
    progress <- progress[progress$progress != "skipped", ]
    expect_equal(nrow(progress), 0L)
  }
})

targets::tar_test("tar_cue_force(TRUE)", {
  targets::tar_script(
    targets::tar_target(x, 0L, cue = tar_cue_force(1 > 0))
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), 0L)
  for (i in seq_len(2)) {
    targets::tar_make(callr_function = NULL)
    progress <- targets::tar_progress()
    progress <- progress[progress$progress != "skipped", ]
    expect_equal(progress$name, "x")
  }
})
