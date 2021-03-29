targets::tar_test("tar_group_select() with empty by", {
  skip_if_not_installed("dplyr")
  expect_error(
    tarchetypes::tar_group_select(data, produce_data()),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_group_select()", {
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
      tarchetypes::tar_group_select(data, produce_data(), starts_with("var")),
      tar_target(group, data, pattern = map(data))
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(length(tar_meta(group)$children[[1]]), 4L)
  for (branch in seq_len(4L)) {
    out <- targets::tar_read(group, branches = branch)
    expect_equal(nrow(out), 3L)
    expect_equal(unique(out$tar_group), branch)
    expect_equal(sort(out$rep), seq_len(3L))
    expect_equal(length(unique(out$var1)), 1L)
    expect_equal(length(unique(out$var2)), 1L)
  }
  out <- targets::tar_read(group)
  expect_equal(nrow(out), 12L)
  expect_equal(nrow(dplyr::distinct(out, var1, var2, rep)), 12L)
})
