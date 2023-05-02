targets::tar_test("tar_download()", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  urls <- c("https://httpbin.org/etag/test", "https://www.r-project.org")
  handle <- curl::new_handle(nobody = TRUE)
  for (url in urls) {
    exists <- tryCatch({
      req <- curl::curl_fetch_memory(as.character(url), handle = handle)
      identical(as.integer(req$status_code), 200L)
    }, error = function(e) FALSE)
    if (!exists) {
      skip("test URL does not exist")
    }
  }
  targets::tar_script({
    list(
      tarchetypes::tar_download(
        x,
        urls = c(
          "https://httpbin.org/etag/test",
          "https://www.r-project.org"
        ),
        paths = c("downloaded_file_1", "downloaded_file_2")
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(
    targets::tar_read(x_url),
    c("https://httpbin.org/etag/test", "https://www.r-project.org")
  )
  expect_equal(
    targets::tar_read(x),
    c("downloaded_file_1", "downloaded_file_2")
  )
  expect_equal(nrow(targets::tar_progress()), 2L)
})
