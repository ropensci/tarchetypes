targets::tar_test("tar_nanoparquet()", {
  skip_if_not_installed("nanoparquet")
  targets::tar_script({
    list(
      tar_nanoparquet(
        name = data,
        command = data.frame(x = 1)
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(data)
  expect_equal(as.data.frame(out), data.frame(x = 1))
  expect_equal(class(out), c("tbl", "data.frame"))
})

targets::tar_test("tar_nanoparquet() alternate options", {
  skip_if_not_installed("nanoparquet")
  targets::tar_script({
    list(
      tar_nanoparquet(
        name = data,
        command = data.frame(x = 1),
        compression = "gzip",
        class = "x"
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(data)
  expect_equal(as.data.frame(out), data.frame(x = 1))
  expect_equal(class(out), c("x", "data.frame"))
})
