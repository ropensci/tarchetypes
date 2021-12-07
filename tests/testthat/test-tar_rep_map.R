targets::tar_test("tar_rep_map() manifest", {
  skip_on_cran()
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
      tarchetypes::tar_rep_map(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_rep_map(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_rep_map(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  suppressWarnings(
    expect_warning(
      out <- targets::tar_manifest(callr_function = NULL),
      class = "tar_condition_deprecate"
    )
  )
  suppressWarnings({
    expect_equal(nrow(out), 8L)
    out <- targets::tar_manifest(label, callr_function = NULL)
    expect_equal(out$command, "\"aggregate\"")
    expect_true(is.na(out$pattern))
    out <- targets::tar_manifest(data1_batch, callr_function = NULL)
    expect_equal(out$command, "seq_len(2)")
    expect_true(is.na(out$pattern))
    out <- targets::tar_manifest(data2_batch, callr_function = NULL)
    expect_equal(out$command, "seq_len(2)")
    expect_true(is.na(out$pattern))
    out <- targets::tar_manifest(data1, callr_function = NULL)
    expect_true(grepl("tar_rep_run", out$command))
    expect_false(is.na(out$pattern))
    out <- targets::tar_manifest(data2, callr_function = NULL)
    expect_true(grepl("tar_rep_run", out$command))
    expect_false(is.na(out$pattern))
    out <- targets::tar_manifest(aggregate1, callr_function = NULL)
    expect_true(grepl("tar_rep2_run", out$command))
    expect_false(is.na(out$pattern))
    out <- targets::tar_manifest(aggregate2, callr_function = NULL)
    expect_true(grepl("tar_rep2_run", out$command))
    expect_false(is.na(out$pattern))
    out <- targets::tar_manifest(aggregate3, callr_function = NULL)
    expect_true(grepl("tar_rep2_run", out$command))
    expect_false(is.na(out$pattern))
  })
})

targets::tar_test("tar_rep_map() graph", {
  skip_on_cran()
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
      tarchetypes::tar_rep_map(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_rep_map(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_rep_map(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  suppressWarnings(
    out <- targets::tar_network(callr_function = NULL)
  )
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

targets::tar_test("tar_rep_map() pipeline", {
  skip_on_cran()
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
      tarchetypes::tar_rep_map(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_rep_map(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_rep_map(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  suppressWarnings(
    targets::tar_make(callr_function = NULL)
  )
  suppressWarnings(
    expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  )
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
})

targets::tar_test("tar_rep_map() runs the command once per rep", {
  skip_on_cran()
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep_map(
        y,
        data.frame(value = rnorm(1)),
        x
      )
    )
  })
  suppressWarnings(
    targets::tar_make(callr_function = NULL)
  )
  out <- targets::tar_read(y)
  expect_equal(nrow(out), 6L)
  expect_false(as.logical(anyDuplicated(out$value)))
})

targets::tar_test("tar_rep_map_raw() runs the command once per rep", {
  skip_on_cran()
  targets::tar_script({
    list(
      tarchetypes::tar_rep(
        x,
        data.frame(value = rnorm(2)),
        batches = 2,
        reps = 3
      ),
      tarchetypes::tar_rep_map_raw(
        name = "y",
        command = quote(data.frame(value = rnorm(1))),
        targets = "x"
      )
    )
  })
  expect_warning(
    targets::tar_make(callr_function = NULL),
    class = "tar_condition_deprecate"
  )
  out <- targets::tar_read(y)
  expect_equal(nrow(out), 6L)
  expect_false(as.logical(anyDuplicated(out$value)))
})

targets::tar_test("tar_rep_map() errors without correct list aggregation", {
  skip_on_cran()
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
      tarchetypes::tar_rep_map(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      )
    )
  })
  suppressWarnings(
    expect_error(
      targets::tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
  suppressWarnings(
    out <- targets::tar_meta(starts_with("aggregate1"), error)
  )
  expect_false(all(is.na(out)))
  expect_true(any(grepl("batch", out)))
  expect_true(any(grepl("iteration", out)))
})

targets::tar_test("tar_rep_map() errors if bad upstream data type", {
  skip_on_cran()
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
      tarchetypes::tar_rep_map(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      )
    )
  })
  suppressWarnings(
    expect_error(
      targets::tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
})
