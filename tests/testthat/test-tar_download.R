targets::tar_test("tar_download() with unequal url/path lengths", {
  expect_error(
    tarchetypes::tar_download(
      x,
      urls = c("https://httpbin.org/etag/test", "https://www.r-project.org"),
      paths = "downloaded_file_1"
    ),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_download() manifest", {
  targets::tar_script({
    list(
      tarchetypes::tar_download(
        x,
        urls = c("https://httpbin.org/etag/test", "https://www.r-project.org"),
        paths = c("downloaded_file_1", "downloaded_file_2")
      )
    )
  })
  out <- targets::tar_manifest(
    fields = c(command, format),
    callr_function = NULL
  )
  expect_equal(nrow(out), 2)
  expect_equal(sort(out$name), sort(c("x_url", "x")))
  out <- targets::tar_manifest(
    x_url,
    fields = c(command, format),
    callr_function = NULL
  )
  expect_true(grepl("httpbin", out$command))
  expect_equal(out$format, "url")
  out <- targets::tar_manifest(
    x,
    fields = c(command, format),
    callr_function = NULL
  )
  expect_true(grepl("tar_download_run", out$command))
  expect_equal(out$format, "file")
})

targets::tar_test("tar_download() graph", {
  targets::tar_script({
    list(
      tarchetypes::tar_download(
        x,
        urls = c("https://httpbin.org/etag/test", "https://www.r-project.org"),
        paths = c("downloaded_file_1", "downloaded_file_2")
      )
    )
  })
  out <- targets::tar_network(callr_function = NULL)$edges
  expect_true(any(out$from == "x_url" & out$to == "x"))
})
