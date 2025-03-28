targets::tar_test("tar_map2() list vs unlist", {
  out <- tar_map2(
    x,
    command1 = f1(arg1),
    command2 = f2(arg1, arg2),
    values = tibble::tibble(arg1 = letters[seq_len(2)]),
    names = arg1,
    suffix1 = "i",
    suffix2 = "ii",
    group = rep(LETTERS[seq_len(2)], each = nrow(!!.x) / 2),
    unlist = TRUE
  )
  expect_equal(
    sort(names(out)),
    sort(
      c(
        "x_i_a", "x_i_b", "x_ii_a", "x_ii_b",
        "x_ii_a_combine", "x_ii_b_combine", "x"
      )
    )
  )
  out <- tar_map2(
    x,
    command1 = f1(arg1),
    command2 = f2(arg1, arg2),
    values = tibble::tibble(arg1 = letters[seq_len(2)]),
    names = arg1,
    suffix1 = "i",
    suffix2 = "ii",
    group = rep(LETTERS[seq_len(2)], each = nrow(!!.x) / 2),
    unlist = FALSE
  )
  expect_equal(
    sort(names(out)),
    sort(c("combine", "combine_dynamic", "static_branches"))
  )
})

targets::tar_test("tar_map2(): combine, columns, static branches", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(LETTERS[seq_len(2)], each = nrow(!!.x) / 2),
      rep_workers = 2
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  out <- out[order(out$name), ]
  expect_equal(
    sort(out$name),
    sort(
      paste0(
        "x",
        c(
          "_i_a", "_i_b", "_ii_a", "_ii_a_combine",
          "_ii_b", "_ii_b_combine", ""
        )
      )
    )
  )
  expect_equal(
    grepl("_i_", out$name),
    grepl("tar_map2_group", out$command)
  )
  expect_equal(
    grepl("_ii_a$", out$name) | grepl("_ii_b$", out$name),
    grepl("tar_map2_run", out$command)
  )
  expect_equal(
    !(out$name %in% c("x_ii_a", "x_ii_b")),
    is.na(out$pattern)
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
    "x_ii_a", "x_ii_a_combine",
    "x_ii_b", "x_ii_b_combine",
    "x_ii_a_combine", "x",
    "x_ii_b_combine", "x"
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
  expect_equal(dim(x), c(8L, 10L))
  expect_true(is.numeric(x$tar_batch))
  expect_true(is.numeric(x$tar_rep))
  expect_true(is.numeric(x$tar_seed))
  out <- dplyr::arrange(x, arg1, arg2)
  expect_equal(out$tar_group, rep(c(1L, 2L), each = 4L))
  out$tar_group <- NULL
  exp <- dplyr::arrange(dplyr::bind_rows(x_ii_a, x_ii_b), arg1, arg2)
  exp$tar_group <- NULL
  expect_equal(out, exp)
  expect_equal(
    x$result,
    c("a 1", "a 2", "a 3", "a 4", "b 1", "b 2", "b 3", "b 4")
  )
  expect_equal(x$length_arg1, rep(1L, 8L))
  expect_equal(x$length_arg2, rep(1L, 8L))
  expect_equal(x$result, paste(x$arg1, x$arg2))
  expect_equal(length(unique(x$random)), nrow(x))
  # upstream output
  expect_equal(dim(x_i_a), c(4L, 3L))
  expect_equal(dim(x_i_b), c(4L, 3L))
  expect_equal(x_i_a$arg1, rep("a", 4L))
  expect_equal(x_i_b$arg1, rep("b", 4L))
  expect_equal(x_i_a$arg2, seq_len(4L))
  expect_equal(x_i_b$arg2, seq_len(4L))
  expect_equal(x_i_a$tar_group, c(1L, 1L, 2L, 2L))
  expect_equal(x_i_b$tar_group, c(1L, 1L, 2L, 2L))
  # metadata
  expect_equal(length(unlist(tar_meta(x_ii_a)$children)), 2L)
  expect_equal(length(unlist(tar_meta(x_ii_b)$children)), 2L)
})

targets::tar_test("tar_map2(): no combine, no columns, static branches", {
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      columns1 = NULL,
      columns2 = NULL,
      group = rep(LETTERS[seq_len(3)], each = nrow(!!.x) / 3),
      combine = FALSE,
      rep_workers = 1
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  out <- out[order(out$name), ]
  expect_equal(nrow(out), 4L)
  expect_equal(
    sort(out$name),
    sort(
      paste0("x", c("_1_a", "_1_b", "_2_a", "_2_b"))
    )
  )
  expect_equal(
    grepl("_1_", out$name),
    grepl("tar_map2_group", out$command)
  )
  expect_equal(
    grepl("_2_", out$name),
    grepl("tar_map2_run", out$command)
  )
  expect_equal(
    grepl("^x$|^x_1_", out$name),
    is.na(out$pattern)
  )
  expect_equal(
    grepl("^x_2", out$name),
    !is.na(out$pattern)
  )
  # network
  out <- targets::tar_network(callr_function = NULL)$edges
  out <- dplyr::arrange(out, from, to)
  exp <- tibble::tribble(
    ~from, ~to,
    "f1", "x_1_a",
    "f1", "x_1_b",
    "f2", "x_2_a",
    "f2", "x_2_b",
    "x_1_a", "x_2_a",
    "x_1_b", "x_2_b",
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # upstream output
  targets::tar_make(callr_function = NULL)
  targets::tar_load(
    tidyselect::any_of(
      c("x_1_a", "x_1_b", "x_2_a", "x_2_b")
    )
  )
  expect_equal(dim(x_1_a), c(6L, 3L))
  expect_equal(dim(x_1_b), c(6L, 3L))
  expect_equal(x_1_a$arg1, rep("a", 6L))
  expect_equal(x_1_b$arg1, rep("b", 6L))
  expect_equal(x_1_a$arg2, seq_len(6L))
  expect_equal(x_1_b$arg2, seq_len(6L))
  expect_equal(x_1_a$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  expect_equal(x_1_b$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  # downstream output
  expect_equal(dim(x_2_a), c(6L, 7L))
  expect_equal(dim(x_2_b), c(6L, 7L))
  expect_equal(x_2_a$result, paste("a", seq_len(6L)))
  expect_equal(x_2_b$result, paste("b", seq_len(6L)))
  expect_equal(x_2_a$length_arg1, rep(1L, 6L))
  expect_equal(x_2_b$length_arg1, rep(1L, 6L))
  expect_equal(x_2_a$length_arg2, rep(1L, 6L))
  expect_equal(x_2_b$length_arg2, rep(1L, 6L))
  expect_equal(length(unique(x_2_a$random)), 6L)
  expect_equal(length(unique(x_2_b$random)), 6L)
  # metadata
  expect_equal(length(unlist(tar_meta(x_2_a)$children)), 3L)
  expect_equal(length(unlist(tar_meta(x_2_b)$children)), 3L)
})

targets::tar_test("tar_map2() columns1", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        value_arg1 = arg1,
        value_arg2 = seq_len(6)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(value_arg1, value_arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      columns1 = arg1,
      columns2 = NULL,
      group = rep(LETTERS[seq_len(3)], each = nrow(!!.x) / 3),
      combine = FALSE
    )
  })
  # upstream output
  targets::tar_make(callr_function = NULL)
  targets::tar_load(
    tidyselect::any_of(
      c("x_1_a", "x_1_b", "x_2_a", "x_2_b")
    )
  )
  expect_equal(dim(x_1_a), c(6L, 4L))
  expect_equal(dim(x_1_b), c(6L, 4L))
  expect_equal(x_1_a$arg1, rep("a", 6L))
  expect_equal(x_1_b$arg1, rep("b", 6L))
  expect_equal(x_1_a$value_arg1, rep("a", 6L))
  expect_equal(x_1_b$value_arg1, rep("b", 6L))
  expect_equal(x_1_a$value_arg2, seq_len(6L))
  expect_equal(x_1_b$value_arg2, seq_len(6L))
  expect_equal(x_1_a$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  expect_equal(x_1_b$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  # downstream output
  expect_equal(dim(x_2_a), c(6L, 8L))
  expect_equal(dim(x_2_b), c(6L, 8L))
  expect_equal(x_2_a$arg1, rep("a", 6L))
  expect_equal(x_2_b$arg1, rep("b", 6L))
  expect_equal(x_2_a$result, paste("a", seq_len(6L)))
  expect_equal(x_2_b$result, paste("b", seq_len(6L)))
  expect_equal(x_2_a$length_arg1, rep(1L, 6L))
  expect_equal(x_2_b$length_arg1, rep(1L, 6L))
  expect_equal(x_2_a$length_arg2, rep(1L, 6L))
  expect_equal(x_2_b$length_arg2, rep(1L, 6L))
  expect_equal(length(unique(x_2_a$random)), 6L)
  expect_equal(length(unique(x_2_b$random)), 6L)
  # metadata
  expect_equal(length(unlist(tar_meta(x_2_a)$children)), 3L)
  expect_equal(length(unlist(tar_meta(x_2_b)$children)), 3L)
})

targets::tar_test("tar_map2() columns2", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        value_arg1 = arg1,
        value_arg2 = seq_len(6)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(value_arg1, value_arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      columns1 = NULL,
      columns2 = value_arg1,
      group = rep(LETTERS[seq_len(3)], each = nrow(!!.x) / 3),
      combine = FALSE
    )
  })
  # upstream output
  targets::tar_make(callr_function = NULL)
  targets::tar_load(
    tidyselect::any_of(
      c("x_1_a", "x_1_b", "x_2_a", "x_2_b")
    )
  )
  expect_equal(dim(x_1_a), c(6L, 3L))
  expect_equal(dim(x_1_b), c(6L, 3L))
  expect_equal(x_1_a$value_arg1, rep("a", 6L))
  expect_equal(x_1_b$value_arg1, rep("b", 6L))
  expect_equal(x_1_a$value_arg2, seq_len(6L))
  expect_equal(x_1_b$value_arg2, seq_len(6L))
  expect_equal(x_1_a$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  expect_equal(x_1_b$tar_group, rep(c(1L, 2L, 3L), each = 2L))
  # downstream output
  expect_equal(dim(x_2_a), c(6L, 8L))
  expect_equal(dim(x_2_b), c(6L, 8L))
  expect_equal(x_2_a$value_arg1, rep("a", 6L))
  expect_equal(x_2_b$value_arg1, rep("b", 6L))
  expect_equal(x_2_a$result, paste("a", seq_len(6L)))
  expect_equal(x_2_b$result, paste("b", seq_len(6L)))
  expect_equal(x_2_a$length_arg1, rep(1L, 6L))
  expect_equal(x_2_b$length_arg1, rep(1L, 6L))
  expect_equal(x_2_a$length_arg2, rep(1L, 6L))
  expect_equal(x_2_b$length_arg2, rep(1L, 6L))
  expect_equal(length(unique(x_2_a$random)), 6L)
  expect_equal(length(unique(x_2_b$random)), 6L)
  # metadata
  expect_equal(length(unlist(tar_meta(x_2_a)$children)), 3L)
  expect_equal(length(unlist(tar_meta(x_2_b)$children)), 3L)
})

targets::tar_test("tar_map2(): no static branches", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = letters[seq_len(4)],
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = NULL,
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      combine = TRUE,
      group = rep(LETTERS[seq_len(2)], each = nrow(!!.x) / 2)
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  out <- out[order(out$name), ]
  expect_equal(nrow(out), 2L)
  expect_equal(
    sort(out$name),
    sort(
      paste0("x", c("_i", "_ii"))
    )
  )
  expect_equal(
    grepl("_i$", out$name),
    grepl("tar_map2_group", out$command)
  )
  expect_equal(
    grepl("_ii", out$name),
    grepl("tar_map2_run", out$command)
  )
  expect_equal(
    grepl("^x_i$", out$name),
    is.na(out$pattern)
  )
  expect_equal(
    grepl("^x_ii", out$name),
    !is.na(out$pattern)
  )
  # network
  out <- targets::tar_network(callr_function = NULL)$edges
  out <- dplyr::arrange(out, from, to)
  exp <- tibble::tribble(
    ~from, ~to,
    "f1", "x_i",
    "f2", "x_ii",
    "x_i", "x_ii"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # output
  targets::tar_make(callr_function = NULL)
  targets::tar_load(tidyselect::any_of(c("x_i", "x_ii")))
  expect_equal(dim(x_i), c(4L, 3L))
  expect_equal(x_i$arg1, letters[seq_len(4L)])
  expect_equal(x_i$arg2, seq_len(4L))
  expect_equal(x_i$tar_group, c(1L, 1L, 2L, 2L))
  expect_equal(dim(x_ii), c(4L, 10L))
  expect_equal(x_ii$result, c("a 1", "b 2", "c 3", "d 4"))
  expect_equal(x_ii$length_arg1, rep(1L, 4L))
  expect_equal(x_ii$length_arg2, rep(1L, 4L))
  expect_equal(length(unique(x_ii$random)), 4L)
  expect_equal(x_ii$arg1, letters[seq_len(4L)])
  expect_equal(x_ii$arg2, seq_len(4L))
  expect_equal(x_ii$tar_group, c(1L, 1L, 2L, 2L))
  # metadata
  expect_equal(length(unlist(tar_meta(x_ii)$children)), 2L)
})

targets::tar_test("tar_map2() column precedence", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function() {
      tibble::tibble(
        fun1 = TRUE,
        fun2 = FALSE
      )
    }
    f2 <- function() {
      tibble::tibble(
        fun1 = FALSE,
        fun2 = TRUE
      )
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(),
      command2 = f2(),
      values = tibble::tibble(
        fun1 = FALSE,
        fun2 = FALSE
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  tar_make(callr_function = NULL)
  tar_load(x)
  expect_equal(x$fun1, FALSE)
  expect_equal(x$fun2, TRUE)
})

targets::tar_test("tar_map2() column precedence 2", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function() {
      tibble::tibble(
        fun1 = TRUE,
        fun2 = FALSE
      )
    }
    f2 <- function() {
      tibble::tibble(
        fun2 = TRUE
      )
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(),
      command2 = f2(),
      values = tibble::tibble(
        fun1 = FALSE,
        scenario = 1
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  tar_make(callr_function = NULL)
  tar_load(x)
  expect_equal(x$fun1, FALSE)
  expect_equal(x$fun2, TRUE)
})

targets::tar_test("tar_map2() column precedence 3", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function() {
      tibble::tibble(
        fun1 = TRUE,
        fun2 = FALSE
      )
    }
    f2 <- function() {
      tibble::tibble(
        fun2 = TRUE
      )
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(),
      command2 = f2(),
      values = tibble::tibble(
        scenario = 1
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  tar_make(callr_function = NULL)
  tar_load(x)
  expect_equal(x$fun1, TRUE)
  expect_equal(x$fun2, TRUE)
})

targets::tar_test("tar_map2() list columns from values", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function() {
      tibble::tibble(
        fun1 = TRUE,
        fun2 = FALSE
      )
    }
    f2 <- function() {
      tibble::tibble(
        fun2 = TRUE
      )
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(),
      command2 = f2(),
      values = tibble::tibble(
        index = c(1L, 2L),
        example = list(c("a", "b"), c("c", "d"))
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  tar_make(callr_function = NULL)
  tar_load(x)
  expect_equal(x$index, c(1L, 2L))
  expect_equal(x$example, list(c("a", "b"), c("c", "d")))
})

targets::tar_test("list column elements from values are selected", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(example) {
      tibble::tibble(length1 = length(example))
    }
    f2 <- function(example) {
      tibble::tibble(length2 = length(example))
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(example),
      command2 = f2(example),
      values = tibble::tibble(
        index = c(1L, 2L),
        example = list(c("a", "b"), c("c", "d"))
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  targets::tar_make(callr_function = NULL)
  targets::tar_load(x)
  expect_equal(x$length1, c(2L, 2L))
  expect_equal(x$length2, c(2L, 2L))
})

targets::tar_test("list column elements from command1 are selected", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(example) {
      tibble::tibble(example2 = list(example))
    }
    f2 <- function(example2) {
      tibble::tibble(length2 = length(example2))
    }
    tarchetypes::tar_map2(
      x,
      command1 = f1(example),
      command2 = f2(example2),
      values = tibble::tibble(
        index = c(1L, 2L),
        example = list(c("a", "b"), c("c", "d"))
      ),
      columns1 = tidyselect::everything(),
      columns2 = tidyselect::everything(),
      group = quote(1)
    )
  })
  targets::tar_make(callr_function = NULL)
  targets::tar_load(x)
  expect_equal(x$length2, c(2L, 2L))
})

targets::tar_test("tar_map2() seed resilience", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(LETTERS[seq_len(2)], each = nrow(!!.x) / 2)
    )
  })
  tar_make(callr_function = NULL)
  out1 <- tar_read(x)
  out1$tar_batch <- NULL
  out1$tar_rep <- NULL
  out1$tar_group <- NULL
  targets::tar_script({
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(1L, nrow(!!.x))
    )
  })
  tar_make(callr_function = NULL)
  out2 <- tar_read(x)
  out2$tar_batch <- NULL
  out2$tar_rep <- NULL
  out2$tar_group <- NULL
  expect_equal(out1, out2)
})

targets::tar_test("tar_map2() seeds change with the seed option", {
  skip_on_cran()
  skip_if(!("seed" %in% names(formals(targets::tar_option_set))))
  targets::tar_script({
    tar_option_set(seed = 1L)
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(1L, nrow(!!.x))
    )
  })
  targets::tar_make(callr_function = NULL)
  out1 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_destroy()
  targets::tar_make(callr_function = NULL)
  out2 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = 2L)
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(1L, nrow(!!.x))
    )
  })
  targets::tar_make(callr_function = NULL)
  out3 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = NA)
    f1 <- function(arg1) {
      tibble::tibble(
        arg1 = arg1,
        arg2 = seq_len(4)
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
    tarchetypes::tar_map2(
      x,
      command1 = f1(arg1),
      command2 = f2(arg1, arg2),
      values = tibble::tibble(arg1 = letters[seq_len(2)]),
      names = arg1,
      suffix1 = "i",
      suffix2 = "ii",
      group = rep(1L, nrow(!!.x))
    )
  })
  targets::tar_make(callr_function = NULL)
  out4 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_make(callr_function = NULL)
  out5 <- paste(unname(targets::tar_read(x)), collapse = " ")
  expect_equal(out1, out2)
  expect_false(out1 == out3)
  expect_false(out1 == out4)
  expect_false(out1 == out5)
  expect_false(out1 == out3)
  expect_false(out3 == out4)
  expect_false(out3 == out5)
  expect_false(out4 == out5)
})
