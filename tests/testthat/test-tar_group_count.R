targets::tar_test("tar_group_count_run()", {
  skip_if_not_installed("dplyr")
  data <- expand.grid(
    var1 = c("a", "b"),
    var2 = c("c", "d"),
    rep = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  out <- tar_group_count_run(data, 12)
  expect_equal(out$tar_group, seq_len(12))
  out <- tar_group_count_run(data, 1)
  expect_equal(out$tar_group, rep(1, 12))
  out <- tar_group_count_run(data, 4)
  expect_equal(out$tar_group, rep(seq_len(4), each = 3))
  out <- tar_group_count_run(data, 3)
  expect_equal(out$tar_group, rep(seq_len(3), each = 4))
  for (count in seq_len(20)) {
    out <- tar_group_count_run(data, count)
    expect_true(is.data.frame(out))
  }
})

targets::tar_test("tar_group_count()", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    produce_data <- function() {
      expand.grid(
        var1 = c("a", "b"),
        var2 = c("c", "d"),
        rep = c(1, 2, 3),
        stringsAsFactors = FALSE
      )
    }
    list(
      tarchetypes::tar_group_count(data, produce_data(), 3),
      tar_target(group, data, pattern = map(data))
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(length(tar_meta(group)$children[[1]]), 3L)
  for (branch in seq_len(3L)) {
    out <- targets::tar_read(group, branches = branch)
    expect_equal(nrow(out), 4L)
  }
  out <- targets::tar_read(group)
  expect_equal(nrow(out), 12L)
  expect_equal(nrow(dplyr::distinct(out, var1, var2, rep)), 12L)
})
