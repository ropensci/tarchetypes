targets::tar_test("tar_file_read() manifest", {
  targets::tar_script({
    write_file <- function() {
      write.csv(mtcars, "mtcars.csv")
      "mtcars.csv"
    }
    get_nrows <- function() {
      2L
    }
    tar_file_read(
      data,
      write_file(),
      utils::read.csv(file = !!.x, nrows = get_nrows())
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 2L)
  out <- targets::tar_manifest(data_file, callr_function = NULL)
  expect_true(grepl("write_file", out$command))
  out <- targets::tar_manifest(data, callr_function = NULL)
  expect_true(grepl("^utils::read\\.csv", out$command))
  expect_true(grepl("data_file", out$command))
  expect_true(grepl("get_nrows", out$command))
})

targets::tar_test("tar_file_read() graph", {
  targets::tar_script({
    write_file <- function() {
      write.csv(mtcars, "mtcars.csv")
      "mtcars.csv"
    }
    get_nrows <- function() {
      2L
    }
    tar_file_read(
      data,
      write_file(),
      utils::read.csv(file = !!.x, nrows = get_nrows())
    )
  })
  out <- targets::tar_network(callr_function = NULL)$edges
  expect_equal(out$to[out$from == "data_file"], "data")
  expect_equal(out$to[out$from == "get_nrows"], "data")
  expect_equal(out$to[out$from == "write_file"], "data_file")
})

targets::tar_test("tar_file_read() pipeline", {
  targets::tar_script({
    write_file <- function() {
      write.csv(mtcars, "mtcars.csv")
      "mtcars.csv"
    }
    get_nrows <- function() {
      2L
    }
    tar_file_read(
      data,
      write_file(),
      utils::read.csv(file = !!.x, nrows = get_nrows())
    )
  })
  targets::tar_make(callr_function = NULL)$edges
  expect_equal(nrow(targets::tar_read(data)), 2L)
})
