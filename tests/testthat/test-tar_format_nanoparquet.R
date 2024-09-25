targets::tar_test("tar_format_nanoparquet()", {
  skip_if_not_installed("nanoparquet")
  targets::tar_script({
    list(
      tar_target(
        name = data,
        command = data.frame(x = 1),
        format = tar_format_nanoparquet()
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(data)
  expect_equal(as.data.frame(out), data.frame(x = 1))
  expect_equal(class(out), c("tbl", "data.frame"))
})

targets::tar_test("tar_format_nanoparquet() alternate options", {
  skip_if_not_installed("nanoparquet")
  targets::tar_script({
    list(
      tar_target(
        name = data,
        command = data.frame(x = 1),
        format = tar_format_nanoparquet(compression = "gzip", class = "x")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(data)
  expect_equal(as.data.frame(out), data.frame(x = 1))
  expect_equal(class(out), c("x", "data.frame"))
})
