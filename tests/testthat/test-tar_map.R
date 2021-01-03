targets::tar_test("tar_map() return value with unlist = TRUE", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    unlist = TRUE
  )
  expect_equal(length(out), 4)
  map(out, ~expect_true(inherits(.x, "tar_target")))
})

targets::tar_test("tar_map() return value with unlist = FALSE", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    unlist = FALSE
  )
  expect_equal(length(out), 2)
  expect_equal(length(out[[1]]), 2)
  expect_equal(length(out[[2]]), 2)
  map(out[[1]], ~expect_true(inherits(.x, "tar_target")))
  map(out[[2]], ~expect_true(inherits(.x, "tar_target")))
})

targets::tar_test("tar_map() with names turned off", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    names = NULL,
    unlist = TRUE
  )
  names <- unname(map_chr(out, ~.x$settings$name))
  expect_equal(sort(names), sort(c("x_1", "x_2", "y_1", "y_2")))
})

targets::tar_test("tar_map() with names misspecified", {
  out <- tar_map(
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    values = list(a = c(12, 34), b = c(56, 78)),
    names = "z",
    unlist = TRUE
  )
  names <- unname(map_chr(out, ~.x$settings$name))
  expect_equal(sort(names), sort(c("x_1", "x_2", "y_1", "y_2")))
})

targets::tar_test("tar_map() manifest", {
  targets::tar_script({
    list(
      tarchetypes::tar_map(
        targets::tar_target(x, a + b),
        targets::tar_target(y, x + a, pattern = map(x)),
        values = list(a = c(12, 34), b = c(56, 78))
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(dim(out), c(4, 3))
  names <- c("x_12_56", "x_34_78", "y_12_56", "y_34_78")
  expect_equal(sort(out$name), sort(c(names)))
  expect_equal(out$command[out$name == "x_12_56"], "12 + 56")
  expect_equal(out$command[out$name == "x_34_78"], "34 + 78")
  expect_equal(out$command[out$name == "y_12_56"], "x_12_56 + 12")
  expect_equal(out$command[out$name == "y_34_78"], "x_34_78 + 34")
  expect_equal(out$pattern[out$name == "x_12_56"], NA_character_)
  expect_equal(out$pattern[out$name == "x_34_78"], NA_character_)
  expect_equal(out$pattern[out$name == "y_12_56"], "map(x_12_56)")
  expect_equal(out$pattern[out$name == "y_34_78"], "map(x_34_78)")
})

targets::tar_test("tar_map() values", {
  targets::tar_script({
    list(
      tarchetypes::tar_map(
        targets::tar_target(x, a + b),
        targets::tar_target(y, x + a, pattern = map(x)),
        values = list(a = c(12, 34), b = c(56, 78))
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(tar_read(x_12_56), 68)
  expect_equal(tar_read(x_34_78), 112)
  expect_equal(tar_read(y_12_56), 80)
  expect_equal(tar_read(y_34_78), 146)
})

targets::tar_test("tar_map(unlist = TRUE) names", {
  out <- tarchetypes::tar_map(
    list(a = c(12, 34), b = c(45, 78)),
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    unlist = TRUE
  )
  exp <- unname(map_chr(out, ~.x$settings$name))
  expect_equal(names(out), exp)
})

targets::tar_test("tar_map(unlist = FALSE) names", {
  out <- tarchetypes::tar_map(
    list(a = c(12, 34), b = c(45, 78)),
    targets::tar_target(x, a + b),
    targets::tar_target(y, x + a, pattern = map(x)),
    unlist = FALSE
  )
  expect_equal(sort(names(out)), sort(c("x", "y")))
  names_x <- unname(map_chr(out$x, ~.x$settings$name))
  names_y <- unname(map_chr(out$y, ~.x$settings$name))
  expect_equal(length(names_x), length(names_y))
  expect_true(all(grepl("^x_", names_x)))
  expect_true(all(grepl("^y_", names_y)))
})
