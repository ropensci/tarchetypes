targets::tar_test("tar_rep_index() without a pipeline", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  expect_error(tar_rep_index(), class = "tar_condition_run")
})

targets::tar_test("tar_rep_index() in tar_rep()", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  targets::tar_script(
    tar_rep(x, data.frame(index = tar_rep_index()), batches = 2L, reps = 3L)
  )
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  expect_equal(
    x$index,
    x$tar_rep + (3L * (x$tar_batch - 1L))
  )
})

targets::tar_test("tar_rep_index() in tar_rep2()", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  targets::tar_script({
    list(
      tar_rep(x, data.frame(index = tar_rep_index()), batches = 2L, reps = 3L),
      tar_rep2(
        name = y,
        command = data.frame(index2 = tar_rep_index(), index = x$index),
        x
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  expect_equal(
    x$index,
    x$tar_rep + (3L * (x$tar_batch - 1L))
  )
  y <- targets::tar_read(y)
  expect_equal(
    y$index,
    y$tar_rep + (3L * (y$tar_batch - 1L))
  )
  expect_equal(y$index, y$index2)
})

targets::tar_test("tar_rep_index() in tar_map_rep()", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  targets::tar_script(
    tar_map_rep(
      x,
      data.frame(index = tar_rep_index()),
      batches = 2L,
      reps = 3L,
      values = list(value = c("a", "b"))
    )
  )
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  expect_equal(nrow(x), 12L)
  expect_equal(
    x$index,
    x$tar_rep + (3L * (x$tar_batch - 1L))
  )
})

targets::tar_test("tar_rep_index() in tar_map2_count()", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(6)
      )
    }
    f2 <- function(arg1, arg2) {
      tibble::tibble(
        result = paste(arg1, arg2),
        length_arg1 = length(arg1),
        length_arg2 = length(arg2),
        random = sample.int(1e6, size = 1L),
        index = tar_rep_index()
      )
    }
    tar_map2_count(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      batches = 3
    )
  })
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  expect_equal(nrow(x), 12L)
  expect_equal(
    x$index,
    x$tar_rep + (2L * (x$tar_batch - 1L))
  )
})

targets::tar_test("tar_rep_index() in tar_map2_count()", {
  skip_on_cran()
  step_reset(step_tar_rep)
  on.exit(step_reset(step_tar_rep))
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(6)
      )
    }
    f2 <- function(arg1, arg2) {
      tibble::tibble(
        result = paste(arg1, arg2),
        length_arg1 = length(arg1),
        length_arg2 = length(arg2),
        random = sample.int(1e6, size = 1L),
        index = tar_rep_index()
      )
    }
    tar_map2_size(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      size = 2
    )
  })
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  expect_equal(nrow(x), 12L)
  expect_equal(
    x$index,
    x$tar_rep + (2L * (x$tar_batch - 1L))
  )
})
