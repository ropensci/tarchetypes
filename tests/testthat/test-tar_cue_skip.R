targets::tar_test("tar_cue_skip(FALSE)", {
  targets::tar_script(
    targets::tar_target(x, 0L, cue = tar_cue_skip(0 > 1))
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), 0L)
  targets::tar_script(
    targets::tar_target(x, 1L, cue = tar_cue_skip(0 > 0))
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_progress()$name, "x")
  targets::tar_make(callr_function = NULL)
  expect_equal(nrow(targets::tar_progress()), 0L)
})

targets::tar_test("tar_cue_skip(TRUE)", {
  targets::tar_script(
    targets::tar_target(x, 0L, cue = tar_cue_skip(1 > 0))
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), 0L)
  targets::tar_script(
    targets::tar_target(x, 1L, cue = tar_cue_skip(1 > 0))
  )
  for (index in seq_len(2L)) {
    targets::tar_make(callr_function = NULL)
    expect_equal(nrow(targets::tar_progress()), 0L)
  }
})
