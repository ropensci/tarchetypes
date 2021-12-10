targets::tar_test("tar_map2_size()", {
  skip_if_not_installed("dplyr")
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
        random = sample.int(1e6, size = 1L)
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
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  out <- out[order(out$name), ]
  expect_equal(
    sort(out$name),
    sort(
      paste0("x", c("_i_a", "_i_b", "_ii_a", "_ii_b", ""))
    )
  )
  expect_equal(
    grepl("_i_", out$name),
    grepl("tar_map2_group", out$command)
  )
  expect_equal(
    grepl("_ii_", out$name),
    grepl("tar_map2_run", out$command)
  )
  expect_equal(
    grepl("^x$|^x_i_", out$name),
    is.na(out$pattern)
  )
  expect_equal(
    grepl("^x_ii", out$name),
    !is.na(out$pattern)
  )
  expect_equal(
    out$name == "x",
    grepl("bind_rows", out$command)
  )
  # network
  out <- targets::tar_network(callr_function = NULL)$edges
  out <- dplyr::arrange(out, from, to)
  exp <- tibble::tribble(
    ~from, ~to,
    "f1", "x_i_a",
    "f1", "x_i_b",
    "f2", "x_ii_a",
    "f2", "x_ii_b",
    "x_i_a", "x_ii_a",
    "x_i_b", "x_ii_b",
    "x_ii_a", "x",
    "x_ii_b", "x"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # downstream output
  targets::tar_make(callr_function = NULL)
  targets::tar_load(
    tidyselect::any_of(
      c("x", "x_i_a", "x_i_b", "x_ii_a", "x_ii_b")
    )
  )
  expect_equal(dim(x), c(12L, 7L))
  out <- dplyr::arrange(x, arg1, arg2)
  expect_equal(out$tar_group, rep(c(1L, 2L), each = 6L))
  out$tar_group <- NULL
  exp <- dplyr::arrange(dplyr::bind_rows(x_ii_a, x_ii_b), arg1, arg2)
  exp$tar_group <- NULL
  expect_equal(out, exp)
  expect_equal(
    x$result,
    c(
      "a 1", "a 2", "a 3", "a 4", "a 5", "a 6",
      "b 1", "b 2", "b 3", "b 4", "b 5", "b 6"
    )
  )
  expect_equal(x$length_arg1, rep(1L, 12L))
  expect_equal(x$length_arg2, rep(1L, 12L))
  expect_equal(x$result, paste(x$arg1, x$arg2))
  expect_equal(length(unique(x$random)), nrow(x))
  # upstream output
  expect_equal(dim(x_i_a), c(6L, 3L))
  expect_equal(dim(x_i_b), c(6L, 3L))
  expect_equal(x_i_a$arg1, rep("a", 6L))
  expect_equal(x_i_b$arg1, rep("b", 6L))
  expect_equal(x_i_a$arg2, seq_len(6L))
  expect_equal(x_i_b$arg2, seq_len(6L))
  expect_equal(x_i_a$tar_group, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(x_i_b$tar_group, c(1L, 1L, 2L, 2L, 3L, 3L))
  # metadata
  expect_equal(length(unlist(tar_meta(x_ii_a)$children)), 3L)
  expect_equal(length(unlist(tar_meta(x_ii_b)$children)), 3L)
})

targets::tar_test("tar_map2_size() works with one-row output", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = 1
      )
    }
    f2 <- function(arg1, arg2) {
      tibble::tibble(
        result = paste(arg1, arg2),
        length_arg1 = length(arg1),
        length_arg2 = length(arg2),
        random = sample.int(1e6, size = 1L)
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
      size = 3
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_read(x)), 2L)
})
