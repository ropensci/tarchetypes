targets::tar_test("tar_rep2() manifest", {
  targets::tar_script({
    list(
      targets::tar_target(label, "aggregate"),
      tarchetypes::tar_rep(
        data1,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep(
        data2,
        list(value = rnorm(2)),
        batches = 2,
        reps = 3,
        iteration = "list"
      ),
      tarchetypes::tar_rep2(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_rep2(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_rep2(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  expect_empty_pattern <- function(out) {
    if ("pattern" %in% colnames(out)) {
      if (all(is.na(out$pattern))) {
        out$pattern <- NULL
      }
      expect_null(out$pattern)
    }
  }
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 8L)
  out <- targets::tar_manifest(label, callr_function = NULL)
  expect_equal(out$command, "\"aggregate\"")
  expect_empty_pattern(out)
  out <- targets::tar_manifest(data1_batch, callr_function = NULL)
  expect_equal(out$command, "seq_len(2)")
  expect_empty_pattern(out)
  out <- targets::tar_manifest(data2_batch, callr_function = NULL)
  expect_equal(out$command, "seq_len(2)")
  expect_empty_pattern(out)
  out <- targets::tar_manifest(data1, callr_function = NULL)
  expect_true(grepl("tar_rep_run", out$command))
  expect_false(is.na(out$pattern))
  expect_true(length(out$pattern) > 0L && nzchar(out$pattern) > 0L)
  out <- targets::tar_manifest(data2, callr_function = NULL)
  expect_true(grepl("tar_rep_run", out$command))
  expect_false(is.na(out$pattern))
  expect_true(length(out$pattern) > 0L && nzchar(out$pattern) > 0L)
  out <- targets::tar_manifest(aggregate1, callr_function = NULL)
  expect_true(grepl("tar_rep2_run", out$command))
  expect_false(is.na(out$pattern))
  expect_true(length(out$pattern) > 0L && nzchar(out$pattern) > 0L)
  out <- targets::tar_manifest(aggregate2, callr_function = NULL)
  expect_true(grepl("tar_rep2_run", out$command))
  expect_false(is.na(out$pattern))
  expect_true(length(out$pattern) > 0L && nzchar(out$pattern) > 0L)
  out <- targets::tar_manifest(aggregate3, callr_function = NULL)
  expect_true(grepl("tar_rep2_run", out$command))
  expect_false(is.na(out$pattern))
  expect_true(length(out$pattern) > 0L && nzchar(out$pattern) > 0L)
})

targets::tar_test("tar_rep2() graph", {
  targets::tar_script({
    list(
      targets::tar_target(label, "aggregate"),
      tarchetypes::tar_rep(
        data1,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep(
        data2,
        list(value = rnorm(2)),
        batches = 2,
        reps = 3,
        iteration = "list"
      ),
      tarchetypes::tar_rep2(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_rep2(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_rep2(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  out <- targets::tar_network(callr_function = NULL)
  exp <- tibble::tribble(
    ~from, ~to,
    "data1", "aggregate1",
    "data2", "aggregate1",
    "label", "aggregate1",
    "data1", "aggregate2",
    "data2", "aggregate2",
    "aggregate1", "aggregate3",
    "aggregate2", "aggregate3",
    "data1_batch", "data1",
    "data2_batch", "data2"
  )
  skip_if_not_installed("dplyr")
  expect_equal(dplyr::arrange(out$edges, from), dplyr::arrange(exp, from))
})

targets::tar_test("tar_rep2() pipeline", {
  targets::tar_script({
    list(
      targets::tar_target(label, "aggregate"),
      tarchetypes::tar_rep(
        data1,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep(
        data2,
        list(value = rnorm(2)),
        batches = 2,
        reps = 3,
        iteration = "list"
      ),
      tarchetypes::tar_rep2(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2,
        rep_workers = 1
      ),
      tarchetypes::tar_rep2(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list",
        rep_workers = 2
      ),
      tarchetypes::tar_rep2(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  targets::tar_load(tidyselect::everything())
  for (batch in seq_len(2)) {
    for (rep in seq_len(3)) {
      out1 <- aggregate1$value[
        aggregate1$tar_batch == batch & aggregate1$tar_rep == rep
      ]
      exp <- data1$value[data1$tar_batch == batch & data1$tar_rep == rep] +
        data2[[batch]][[rep]]$value
      expect_equal(out1, exp)
      out2 <- aggregate2[[batch]][[rep]]$value
      expect_equal(out2, exp)
      out3 <- aggregate3$value[
        aggregate3$tar_batch == batch & aggregate3$tar_rep == rep
      ]
      expect_equal(out1 + out2, out3)
    }
  }
  out <- tar_read(aggregate1, branches = 2)
  expect_equal(out$tar_batch, rep(2L, 6L))
  expect_equal(out$tar_rep, rep(seq_len(3L), each = 2L))
  expect_true(is.numeric(out$tar_seed))
})

targets::tar_test("tar_rep2() runs the command once per rep", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep2(
        y,
        data.frame(value = rnorm(1)),
        x
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(y)
  expect_equal(nrow(out), 6L)
  expect_false(as.logical(anyDuplicated(out$value)))
})

targets::tar_test("tar_rep2() errors without correct list aggregation", {
  targets::tar_script({
    list(
      targets::tar_target(label, "aggregate"),
      tarchetypes::tar_rep(
        data1,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep(
        data2,
        list(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep2(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      )
    )
  })
  expect_error(
    targets::tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
  out <- targets::tar_meta(starts_with("aggregate1"), error)
  expect_false(all(is.na(out)))
  expect_true(any(grepl("batch", out)))
  expect_true(any(grepl("iteration", out)))
})

targets::tar_test("tar_rep2() errors if bad upstream data type", {
  targets::tar_script({
    list(
      targets::tar_target(label, "aggregate"),
      tarchetypes::tar_rep(
        data1,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep(
        data2,
        rnorm(2),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep2(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      )
    )
  })
  expect_error(
    targets::tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

targets::tar_test("tar_rep2() seeds are resilient to re-batching", {
  skip_on_cran()
  targets::tar_script({
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 1, reps = 4),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out1 <- targets::tar_read(y)
  out1$tar_batch <- NULL
  out1$tar_rep <- NULL
  targets::tar_script({
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 2, reps = 2),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out2 <- targets::tar_read(y)
  out2$tar_batch <- NULL
  out2$tar_rep <- NULL
  targets::tar_script({
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 4, reps = 1),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out3 <- targets::tar_read(y)
  out3$tar_batch <- NULL
  out3$tar_rep <- NULL
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})

targets::tar_test("tar_rep2() seeds change with the seed option", {
  skip_on_cran()
  skip_if(!("seed" %in% names(formals(targets::tar_option_set))))
  targets::tar_script({
    tar_option_set(seed = 1L)
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 2, reps = 2),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out1 <- paste(unname(targets::tar_read(y)), collapse = " ")
  targets::tar_destroy()
  targets::tar_make(callr_function = NULL)
  out2 <- paste(unname(targets::tar_read(y)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = 2L)
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 2, reps = 2),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out3 <- paste(unname(targets::tar_read(y)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = NA)
    f <- function() {
      tibble::tibble(
        x = digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
    }
    g <- function(x) {
      x$x <- paste0(
        x$x,
        digest::digest(
          paste(sample.int(n = 1e9, size = 1000), collapse = "_")
        )
      )
      x
    }
    list(
      tarchetypes::tar_rep(x, f(), batches = 2, reps = 2),
      tarchetypes::tar_rep2(y, g(x), targets = "x")
    )
  })
  targets::tar_make(callr_function = NULL)
  out4 <- paste(unname(targets::tar_read(y)), collapse = " ")
  targets::tar_make(callr_function = NULL)
  out5 <- paste(unname(targets::tar_read(y)), collapse = " ")
  expect_equal(out1, out2)
  expect_false(out1 == out3)
  expect_false(out1 == out4)
  expect_false(out1 == out5)
  expect_false(out1 == out3)
  expect_false(out3 == out4)
  expect_false(out3 == out5)
  expect_false(out4 == out5)
})
