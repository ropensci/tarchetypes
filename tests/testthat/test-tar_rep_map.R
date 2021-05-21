targets::tar_test("tar_map_reps() manifest", {
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
      tarchetypes::tar_map_reps(
        aggregate1,
        data.frame(x = label, value = data1$value + data2$value),
        data1,
        data2
      ),
      tarchetypes::tar_map_reps(
        aggregate2,
        list(value = data1$value + data2$value),
        data1,
        data2,
        iteration = "list"
      ),
      tarchetypes::tar_map_reps(
        aggregate3,
        data.frame(value = aggregate1$value + aggregate2$value),
        aggregate1,
        aggregate2
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
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
  expect_true(grepl("tar_map_reps_run", out$command))
  expect_false(is.na(out$pattern))
  out <- targets::tar_manifest(aggregate2, callr_function = NULL)
  expect_true(grepl("tar_map_reps_run", out$command))
  expect_false(is.na(out$pattern))
  out <- targets::tar_manifest(aggregate3, callr_function = NULL)
  expect_true(grepl("tar_map_reps_run", out$command))
  expect_false(is.na(out$pattern))
})
