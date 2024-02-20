targets::tar_test("tar_map() deep-copies the targets", {
  x <- targets::tar_target(x, a + b)
  out <- tar_map(x, values = list(a = 12, b = 56), unlist = TRUE)
  y <- out[[1]]
  y$cue$command <- FALSE
  y$settings$format <- "file"
  expect_equal(x$cue$command, TRUE)
  expect_equal(x$settings$format, "rds")
})

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
  expect_equal(length(names), 4L)
  expect_equal(length(grep("^x_", names)), 2L)
  expect_equal(length(grep("^y_", names)), 2L)
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
  expect_equal(length(names), 4L)
  expect_equal(length(grep("^x_", names)), 2L)
  expect_equal(length(grep("^y_", names)), 2L)
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
  expect_equal(dim(out), c(4, 4))
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
  expect_equal(unname(tar_read(y_12_56)), 80)
  expect_equal(unname(tar_read(y_34_78)), 146)
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


targets::tar_test("tar_map() and complicated values", {
  tar_script({
    tarchetypes::tar_map(
      list(a = list(c(12, 34), c(45, 78))),
      targets::tar_target(x, a),
      names = NULL
    )
  })
  targets::tar_make(callr_function = NULL)
  manifest <- targets::tar_manifest(callr_function = NULL)
  name1 <- manifest$name[grepl("12", manifest$command)]
  name2 <- manifest$name[grepl("78", manifest$command)]
  expect_equal(targets::tar_read_raw(name1), c(12, 34))
  expect_equal(targets::tar_read_raw(name2), c(45, 78))
})

targets::tar_test("tar_map() is not sensitive to ordering (#67)", {
  targets::tar_script({
    library(tarchetypes)
    tar_map(
      values = list(x = c("a", "b")),
      names = NULL,
      tar_target(y, x)
    )
  })
  manifest1 <- targets::tar_manifest(callr_function = NULL)
  targets::tar_make(callr_function = NULL)
  # Switch the order of the values.
  targets::tar_script({
    library(tarchetypes)
    tar_map(
      values = list(x = c("b", "a")),
      names = NULL,
      tar_target(y, x)
    )
  })
  manifest2 <- targets::tar_manifest(callr_function = NULL)
  expect_equal(manifest1, manifest2)
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_progress()
  expect_equal(unique(out$progress), "skipped")
})

targets::tar_test("tar_map() default descriptions", {
  skip_on_cran()
  targets::tar_script({
    library(targets)
    tar_map(
      values = list(
        name = letters[seq_len(4L)]
      ),
      descriptions = NULL,
      tar_target(x, 1)
    )
  })
  manifest <- targets::tar_manifest(
    callr_function = NULL,
    fields = tidyselect::any_of("description"),
    drop_missing = FALSE
  )
  expect_true(all(is.na(manifest$description)))
})

targets::tar_test("tar_map() description from target only", {
  skip_on_cran()
  targets::tar_script({
    library(targets)
    tar_map(
      values = list(
        name = letters[seq_len(4L)]
      ),
      descriptions = NULL,
      tar_target(x, 1, description = "info")
    )
  })
  manifest <- targets::tar_manifest(
    callr_function = NULL,
    fields = tidyselect::any_of("description"),
    drop_missing = FALSE
  )
  expect_equal(manifest$description, rep("info", 4L))
})

targets::tar_test("tar_map() description from values only", {
  skip_on_cran()
  targets::tar_script({
    library(targets)
    tar_map(
      values = list(
        name = letters[seq_len(4L)],
        blurb = as.character(seq_len(4L))
      ),
      names = tidyselect::any_of("name"),
      descriptions = tidyselect::any_of("blurb"),
      tar_target(x, 1)
    )
  })
  manifest <- targets::tar_manifest(
    callr_function = NULL,
    fields = tidyselect::any_of("description"),
    drop_missing = FALSE
  )
  expect_equal(manifest$description, as.character(seq_len(4L)))
})

targets::tar_test("tar_map() description from both targets and values", {
  skip_on_cran()
  targets::tar_script({
    library(targets)
    tar_map(
      values = list(
        name = letters[seq_len(4L)],
        blurb = as.character(seq_len(4L)),
        blurb2 = c("w", "x", "y", "z")
      ),
      names = tidyselect::any_of("name"),
      descriptions = tidyselect::any_of(c("blurb", "blurb2")),
      tar_target(x, 1, description = "info")
    )
  })
  manifest <- targets::tar_manifest(
    callr_function = NULL,
    fields = tidyselect::any_of("description"),
    drop_missing = FALSE
  )
  expect_equal(
    manifest$description,
    paste("info", seq_len(4L), c("w", "x", "y", "z"))
  )
})
