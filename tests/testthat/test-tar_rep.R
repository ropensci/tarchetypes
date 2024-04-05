targets::tar_test("tar_rep(iteration = 'list', rep_workers = 1)", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        list(x = sample.int(1e4, 1)),
        batches = 2,
        reps = 3,
        iteration = "list",
        rep_workers = 1
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(x)
  expect_equal(length(out), 2)
  expect_equal(length(out[[1]]), 3)
  expect_equal(length(out[[2]]), 3)
  df <- do.call(vctrs::vec_rbind, unlist(out, recursive = FALSE))
  for (col in colnames(df)) {
    df[[col]] <- unlist(df[[col]])
  }
  expect_equal(dim(df), c(6L, 4L))
  expect_equal(
    sort(colnames(df)),
    sort(c("x", "tar_batch", "tar_rep", "tar_seed"))
  )
  expect_true(is.integer(df$x))
  expect_equal(df$tar_batch, rep(seq_len(2), each = 3))
  expect_equal(df$tar_rep, rep(rep(seq_len(3), each = 1), times = 2))
  expect_true(is.numeric(df$tar_seed))
  expect_equal(length(tar_meta(x, fields = "children")$children[[1]]), 2L)
  expect_equal(tar_read(x, branches = 1), out[1])
  expect_equal(tar_read(x, branches = 2), out[2])
})

targets::tar_test("tar_rep(iteration = 'vector', rep_workers = 2)", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(x = sample.int(1e4, 2)),
        batches = 2,
        reps = 3,
        iteration = "vector",
        rep_workers = 2
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(x)
  expect_equal(dim(out), c(12L, 4L))
  expect_equal(
    sort(colnames(out)),
    sort(c("x", "tar_batch", "tar_rep", "tar_seed"))
  )
  expect_true(is.integer(out$x))
  expect_equal(out$tar_batch, rep(seq_len(2), each = 6))
  expect_equal(out$tar_rep, rep(rep(seq_len(3), each = 2), times = 2))
  expect_true(is.numeric(out$tar_seed))
  expect_equal(length(tar_meta(x, fields = "children")$children[[1]]), 2L)
  expect_equal(tar_read(x, branches = 1), out[seq_len(6), ])
  expect_equiv(tar_read(x, branches = 2), out[seq_len(6) + 6, ])
})

targets::tar_test("tar_rep_raw(iteration = 'vector')", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep_raw(
        "x",
        expression(data.frame(x = sample.int(1e4, 2))),
        batches = 2,
        reps = 3,
        iteration = "vector"
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(x)
  expect_equal(dim(out), c(12L, 4L))
  expect_equal(
    sort(colnames(out)),
    sort(c("x", "tar_batch", "tar_rep", "tar_seed"))
  )
  expect_true(is.integer(out$x))
  expect_equal(out$tar_batch, rep(seq_len(2), each = 6))
  expect_equal(out$tar_rep, rep(rep(seq_len(3), each = 2), times = 2))
  expect_equal(length(tar_meta(x, fields = "children")$children[[1]]), 2L)
  expect_equal(tar_read(x, branches = 1), out[seq_len(6), ])
  expect_equiv(tar_read(x, branches = 2), out[seq_len(6) + 6, ])
})

targets::tar_test("tar_rep() with non-list output", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        sample.int(1e4, 1),
        batches = 2,
        reps = 3,
        iteration = "vector"
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- tar_read(x)
  expect_true(is.numeric(out))
  expect_equal(length(out), 6L)
})

targets::tar_test("tar_rep_run() with unsupported iteration method", {
  expect_error(
    tar_rep_run(quote(1), 1, 1, "nope", rep_workers = 1),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_rep() seeds are resilient to re-batching", {
  skip_on_cran()
  targets::tar_script({
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 1, reps = 4)
  })
  targets::tar_make(callr_function = NULL)
  out1 <- unname(targets::tar_read(x))
  targets::tar_script({
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 2, reps = 2)
  })
  targets::tar_make(callr_function = NULL)
  out2 <- unname(targets::tar_read(x))
  targets::tar_script({
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 4, reps = 1)
  })
  targets::tar_make(callr_function = NULL)
  out3 <- unname(targets::tar_read(x))
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})

targets::tar_test("tar_rep() seeds change with the seed option", {
  skip_on_cran()
  skip_if(!("seed" %in% names(formals(targets::tar_option_set))))
  targets::tar_script({
    tar_option_set(seed = 1L)
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 2, reps = 2)
  })
  targets::tar_make(callr_function = NULL)
  out1 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_destroy()
  targets::tar_make(callr_function = NULL)
  out2 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = 2L)
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 2, reps = 2)
  })
  targets::tar_make(callr_function = NULL)
  out3 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = NA)
    f <- function() {
      secretbase::siphash13(
        paste(sample.int(n = 1e9, size = 1000), collapse = "_")
      )
    }
    tarchetypes::tar_rep(x, f(), batches = 2, reps = 2)
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

targets::tar_test("correct RNG state", {
  skip_on_cran()
  targets::tar_script({
    targets::tar_option_set(packages = c("secretbase", "tibble"))
    tar_rep(
      name = results,
      command = tibble(seed_hash = siphash13(.Random.seed)),
      batches = 1L,
      reps = 3L,
      rep_workers = 2L
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- tar_read(results)
  for (rep in seq_len(3L)) {
    set.seed(seed = out$tar_seed[rep], kind = "default")
    expect_equal(out$seed_hash[rep], secretbase::siphash13(.Random.seed))
  }
})

targets::tar_test("tar_rep_bind() group", {
  out <- tar_rep_bind(
    out = list(a = tibble::tibble(a = "x")),
    iteration = "group"
  )
  expect_true(tibble::is_tibble(out))
  expect_equal(colnames(out), "a")
  expect_equal(out$a, "x")
})

targets::tar_test("tar_rep_bind() error handling", {
  expect_error(
    tar_rep_bind(out = "x", iteration = "none"),
    class = "tar_condition_validate"
  )
})
