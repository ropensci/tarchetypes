targets::tar_test("tar_map_rep() with columns and static branches", {
  targets::tar_script({
    f <- function(sigma1, sigma2) {
      tibble::tibble(
        out = sigma1 + 1000 * sigma2
      )
    }
    hyperparameters <- tibble::tibble(
      scenario = c("tight", "medium", "diffuse"),
      sigma1 = c(10, 50, 50),
      sigma2 = c(10, 5, 10)
    )
    tarchetypes::tar_map_rep(
      x,
      command = f(sigma1, sigma2),
      values = hyperparameters,
      names = tidyselect::any_of("scenario"),
      batches = 2,
      reps = 3
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  out <- out[order(out$name), ]
  expect_equal(
    sort(out$name),
    sort(
      paste0("x", c("_batch", "_tight", "_medium", "_diffuse", ""))
    )
  )
  expect_equal(out$command[out$name == "x_batch"], "seq_len(2)")
  expect_equal(
    grepl("diffuse|medium|tight", out$name),
    grepl("tar_rep_run", out$command)
  )
  expect_equal(
    grepl("diffuse|medium|tight", out$name),
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
    "f", "x_diffuse",
    "f", "x_medium",
    "f", "x_tight",
    "x_batch", "x_diffuse",
    "x_batch", "x_medium",
    "x_batch", "x_tight",
    "x_diffuse", "x",
    "x_medium", "x",
    "x_tight", "x"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # output
  targets::tar_make(callr_function = NULL)
  out <- dplyr::arrange(tar_read(x), tar_batch, tar_rep, scenario)
  d <- dplyr::distinct(out, tar_group, tar_batch, tar_rep)
  expect_equal(nrow(out), nrow(d))
  expect_equal(out$out, rep(c(10050, 5050, 10010), times = 6))
  expect_equal(out$sigma1, rep(c(50, 50, 10), times = 6))
  expect_equal(out$sigma2, rep(c(10, 5, 10), times = 6))
  scenarios <- sort(unique(out$scenario))
  expect_equal(out$scenario, rep(scenarios, times = 6))
})
  