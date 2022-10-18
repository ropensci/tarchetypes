targets::tar_test("tar_map_rep(): combine, columns, static branches", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f <- function(sigma1, sigma2) {
      tibble::tibble(
        out = sigma1 + 1000 * sigma2,
        length1 = length(sigma1),
        length2 = length(sigma2),
        random = sample.int(1e6, size = 1)
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
  out <- dplyr::arrange(targets::tar_read(x), tar_batch, tar_rep, scenario)
  d <- dplyr::distinct(out, tar_group, tar_batch, tar_rep)
  expect_equal(nrow(out), nrow(d))
  expect_equal(out$out, rep(c(10050, 5050, 10010), times = 6))
  expect_equal(out$sigma1, rep(c(50, 50, 10), times = 6))
  expect_equal(out$sigma2, rep(c(10, 5, 10), times = 6))
  scenarios <- sort(unique(out$scenario))
  expect_equal(out$scenario, rep(scenarios, times = 6))
  expect_true(all(out$length1 == 1L))
  expect_true(all(out$length2 == 1L))
  expect_equal(length(unique(out$random)), nrow(out))
  # metadata
  meta <- targets::tar_meta(x_diffuse)
  expect_equal(length(unlist(meta$children)), 2L)
})

targets::tar_test("tar_map_rep(): no combine, 1 col, static branches", {
  skip_if_not_installed("dplyr")
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
      columns = tidyselect::any_of(c("scenario", "sigma2")),
      combine = FALSE,
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
      paste0("x", c("_batch", "_tight", "_medium", "_diffuse"))
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
    "x_batch", "x_tight"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # output
  targets::tar_make(callr_function = NULL)
  out <- dplyr::bind_rows(
    targets::tar_read(x_diffuse),
    targets::tar_read(x_medium),
    targets::tar_read(x_tight)
  )
  out <- dplyr::arrange(out, tar_batch, tar_rep, scenario)
  d <- dplyr::distinct(out, tar_batch, tar_rep)
  expect_equal(nrow(out), nrow(d) * 3)
  expect_equal(out$out, rep(c(10050, 5050, 10010), times = 6))
  expect_false("sigma1" %in% colnames(out))
  expect_equal(out$sigma2, rep(c(10, 5, 10), times = 6))
  scenarios <- sort(unique(out$scenario))
  expect_equal(out$scenario, rep(scenarios, times = 6))
  # metadata
  meta <- targets::tar_meta(x_diffuse)
  expect_equal(length(unlist(meta$children)), 2L)
})

targets::tar_test("tar_map_rep(): combine, no cols, static branches", {
  skip_if_not_installed("dplyr")
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
      columns = NULL,
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
  out <- dplyr::arrange(targets::tar_read(x), tar_batch, tar_rep, tar_group)
  d <- dplyr::distinct(out, tar_group, tar_batch, tar_rep)
  expect_equal(nrow(out), nrow(d))
  expect_equal(out$out, rep(c(10050, 5050, 10010), times = 6))
  expect_equal(
    sort(colnames(out)),
    sort(c("tar_group", "out", "tar_batch", "tar_rep", "tar_seed"))
  )
  # metadata
  meta <- targets::tar_meta(x_diffuse)
  expect_equal(length(unlist(meta$children)), 2L)
})

targets::tar_test("tar_map_rep(): no static branches", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f <- function(sigma1, sigma2) {
      tibble::tibble(
        out = sigma1 + 1000 * sigma2
      )
    }
    tarchetypes::tar_map_rep(
      x,
      command = f(1, 2),
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
      paste0("x", c("_batch", ""))
    )
  )
  expect_equal(out$command[out$name == "x_batch"], "seq_len(2)")
  expect_equal(
    grepl("^x$", out$name),
    grepl("tar_rep_run", out$command)
  )
  expect_equal(
    grepl("^x$", out$name),
    !is.na(out$pattern)
  )
  # network
  out <- targets::tar_network(callr_function = NULL)$edges
  out <- dplyr::arrange(out, from, to)
  exp <- tibble::tribble(
    ~from, ~to,
    "f", "x",
    "x_batch", "x"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # output
  targets::tar_make(callr_function = NULL)
  out <- dplyr::arrange(targets::tar_read(x), tar_batch, tar_rep)
  d <- dplyr::distinct(out, tar_batch, tar_rep)
  expect_equal(nrow(out), nrow(d))
  expect_equal(out$out, rep(2001, times = 6))
  expect_equal(
    sort(colnames(out)),
    sort(c("out", "tar_batch", "tar_rep", "tar_seed"))
  )
  # metadata
  meta <- targets::tar_meta(x)
  expect_equal(length(unlist(meta$children)), 2L)
})

targets::tar_test("tar_map_rep() column precedence", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f <- function(theta) {
      tibble::tibble(
        theta = theta + 1L
      )
    }
    hyperparameters <- tibble::tibble(theta = 1L)
    tarchetypes::tar_map_rep(
      x,
      command = f(theta),
      values = hyperparameters,
      names = tidyselect::any_of("scenario"),
      batches = 2,
      reps = 3
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(unique(targets::tar_read(x)$theta), 2L)
})

targets::tar_test("tar_map_rep() list column support", {
  skip_if_not_installed("dplyr")
  targets::tar_script({
    f <- function(theta) {
      tibble::tibble(
        value = 1
      )
    }
    hyperparameters <- tibble::tibble(
      index = c(1L, 2L),
      theta = list(c(1L, 2L), c(3L, 4L))
    )
    tarchetypes::tar_map_rep(
      x,
      command = f(theta),
      values = hyperparameters,
      columns = tidyselect::everything(),
      batches = 1,
      reps = 2
    )
  })
  targets::tar_make(callr_function = NULL)
  x <- targets::tar_read(x)
  x <- dplyr::arrange(x, index, tar_rep)
  expect_equal(x$index, c(1L, 1L, 2L, 2L))
  expect_equal(
    tar_read(x)$theta,
    list(c(1L, 2L), c(1L, 2L), c(3L, 4L), c(3L, 4L))
  )
})

targets::tar_test("tar_map_rep() seeds are resilient to re-batching", {
  skip_on_cran()
  targets::tar_script({
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 1,
      reps = 4
    )
  })
  targets::tar_make(callr_function = NULL)
  out1 <- targets::tar_read(x)
  out1$tar_batch <- NULL
  out1$tar_rep <- NULL
  targets::tar_script({
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 2,
      reps = 2
    )
  })
  targets::tar_make(callr_function = NULL)
  out2 <- targets::tar_read(x)
  out2$tar_batch <- NULL
  out2$tar_rep <- NULL
  targets::tar_script({
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 4,
      reps = 1
    )
  })
  targets::tar_make(callr_function = NULL)
  out3 <- targets::tar_read(x)
  out3$tar_batch <- NULL
  out3$tar_rep <- NULL
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})

targets::tar_test("tar_map_rep() seeds change with the seed option", {
  skip_on_cran()
  skip_if(!("seed" %in% names(formals(targets::tar_option_set))))
  targets::tar_script({
    tar_option_set(seed = 1L)
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 2,
      reps = 2
    )
  })
  targets::tar_make(callr_function = NULL)
  out1 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_destroy()
  targets::tar_make(callr_function = NULL)
  out2 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = 2L)
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 2,
      reps = 2
    )
  })
  targets::tar_make(callr_function = NULL)
  out3 <- paste(unname(targets::tar_read(x)), collapse = " ")
  targets::tar_script({
    tar_option_set(seed = NA)
    f <- function(x) {
      out <- digest::digest(
        paste(c(x, sample.int(n = 1e9, size = 1000)), collapse = "_")
      )
      data.frame(x = out)
    }
    tarchetypes::tar_map_rep(
      x,
      f(a),
      values = list(a = c(1, 2)),
      batches = 2,
      reps = 2
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
