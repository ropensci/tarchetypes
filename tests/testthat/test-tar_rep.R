targets::tar_test("tar_rep(iteration = 'list')", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        list(x = sample.int(1e4, 1)),
        batches = 2,
        reps = 3,
        iteration = "list"
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
  expect_equal(dim(df), c(6L, 3L))
  expect_equal(sort(colnames(df)), sort(c("x", "tar_batch", "tar_rep")))
  expect_true(is.integer(df$x))
  expect_equal(df$tar_batch, rep(seq_len(2), each = 3))
  expect_equal(df$tar_rep, rep(rep(seq_len(3), each = 1), times = 2))
  expect_equal(length(tar_meta(x, fields = "children")$children[[1]]), 2L)
  expect_equal(tar_read(x, branches = 1), out[1])
  expect_equal(tar_read(x, branches = 2), out[2])
})

targets::tar_test("tar_rep(iteration = 'vector')", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(x = sample.int(1e4, 2)),
        batches = 2,
        reps = 3,
        iteration = "vector"
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(x)
  expect_equal(dim(out), c(12L, 3L))
  expect_equal(sort(colnames(out)), sort(c("x", "tar_batch", "tar_rep")))
  expect_true(is.integer(out$x))
  expect_equal(out$tar_batch, rep(seq_len(2), each = 6))
  expect_equal(out$tar_rep, rep(rep(seq_len(3), each = 2), times = 2))
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
  expect_equal(dim(out), c(12L, 3L))
  expect_equal(sort(colnames(out)), sort(c("x", "tar_batch", "tar_rep")))
  expect_true(is.integer(out$x))
  expect_equal(out$tar_batch, rep(seq_len(2), each = 6))
  expect_equal(out$tar_rep, rep(rep(seq_len(3), each = 2), times = 2))
  expect_equal(length(tar_meta(x, fields = "children")$children[[1]]), 2L)
  expect_equal(tar_read(x, branches = 1), out[seq_len(6), ])
  expect_equiv(tar_read(x, branches = 2), out[seq_len(6) + 6, ])
})

targets::tar_test("tar_rep(iteration = 'group')", {
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(x = sample.int(1e4, 2), tar_group = 1),
        batches = 2,
        reps = 3,
        iteration = "group"
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(x)
  expect_equal(dim(out), c(12L, 4L))
  expect_equal(
    sort(colnames(out)),
    sort(c("x", "tar_batch", "tar_rep", "tar_group"))
  )
  expect_true(is.integer(out$x))
  expect_equal(out$tar_batch, rep(seq_len(2), each = 6))
  expect_equal(out$tar_rep, rep(rep(seq_len(3), each = 2), times = 2))
  expect_equal(out$tar_group, rep(1, 12))
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
    tar_rep_run(quote(1), 1, 1, "nope"),
    class = "condition_validate"
  )
})
