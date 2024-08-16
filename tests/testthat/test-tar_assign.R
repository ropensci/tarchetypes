targets::tar_test("tar_assign() single statement", {
  targets::tar_script(tar_assign(x <- tar_target(c(1L, 2L))))
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "c(1L, 2L)")
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), c(1L, 2L))
  targets::tar_script(tar_assign({x = tar_target(c(1L, 2L))})) # nolint
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "c(1L, 2L)")
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), c(1L, 2L))
})

targets::tar_test("tar_assign()", {
  targets::tar_script({
    tar_assign({
      x = tar_target(c(1L, 2L)) # nolint
      y <- tar_target(x + 1L, pattern = map(x))
      z <- tar_rep(TRUE, batches = 2L)
    })
  })
  out <- targets::tar_manifest(names = x, callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "c(1L, 2L)")
  out <- targets::tar_manifest(names = y, callr_function = NULL)
  expect_equal(out$name, "y")
  expect_equal(out$command, "x + 1L")
  expect_equal(out$pattern, "map(x)")
  out <- targets::tar_manifest(names = z, callr_function = NULL)
  expect_equal(out$name, "z")
  expect_true(grepl("^tarchetypes::tar_rep_run\\(", out$command))
  expect_equal(out$pattern, "map(z_batch)")
  out <- targets::tar_manifest(names = z_batch, callr_function = NULL)
  expect_equal(out$name, "z_batch")
  expect_equal(out$command, "seq_len(2L)")
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(x), c(1L, 2L))
  expect_equal(unname(targets::tar_read(y)), c(2L, 3L))
  expect_equal(unname(targets::tar_read(y, branches = 1L)), 2L)
  expect_equal(unname(targets::tar_read(y, branches = 2L)), 3L)
  expect_equal(targets::tar_read(z_batch), c(1L, 2L))
  expect_equal(unname(targets::tar_read(z)), rep(TRUE, 2L))
})

targets::tar_test("tar_assign() non-calls", {
  expect_error(
    tar_assign({
      x <- targets::tar_target(1)
      y <- 3
      z <- tar_target(x + y)
    }),
    class = "tar_condition_validate"
  )
})
