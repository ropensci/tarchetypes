targets::tar_test("tar_hook_outer() deep-copies the targets", {
  x <- targets::tar_target(x1, task1())
  y <- tar_hook_outer(x, f(.x))[[1]]
  y$cue$command <- FALSE
  y$settings$format <- "file"
  expect_equal(x$cue$command, TRUE)
  expect_equal(x$settings$format, "rds")
})

targets::tar_test("tar_hook_outer() requires .x", {
  x <- tar_target(x, 1)
  expect_error(
    tar_hook_outer(x, f()),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_hook_outer() inserts code", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_outer(
      targets = targets,
      hook = f(.x, "Running hook."),
      names = NULL
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_true(all(grepl("Running hook", out$command)))
})

targets::tar_test("tar_hook_outer() with tidyselect", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_outer(
      targets = targets,
      hook = f(.x, "Running hook."),
      names = tidyselect::starts_with("x")
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_equal(
    grepl("Running hook", out$command),
    grepl("^x", out$name)
  )
})

targets::tar_test("tar_hook_outer() changes internals properly", {
  skip_on_cran()
  skip_if(!exists("tar_resources", getNamespace("targets")))
  resources <- targets::tar_resources(qs = targets::tar_resources_qs())
  x <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = resources
  )
  y <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = resources
  )
  for (field in c("packages", "library", "deps", "seed", "string", "hash")) {
    expect_equal(x$command[[field]], y$command[[field]])
  }
  for (field in setdiff(names(x$settings), "pattern")) {
    expect_equal(x$settings[[field]], y$settings[[field]])
  }
  expect_equal(deparse(x$settings$pattern), deparse(y$settings$pattern))
  for (field in names(x$cue)) {
    expect_equal(x$cue[[field]], y$cue[[field]])
  }
  expect_equal(x$store$resources, y$store$resources)
  # Apply the hook.
  y <- tar_hook_outer(y, f(.x))[[1]]
  # Most elements should stay the same
  for (field in c("packages", "library", "seed")) {
    expect_equal(x$command[[field]], y$command[[field]])
  }
  for (field in setdiff(names(x$settings), "pattern")) {
    expect_equal(x$settings[[field]], y$settings[[field]])
  }
  expect_equal(deparse(x$settings$pattern), deparse(y$settings$pattern))
  for (field in names(x$cue)) {
    expect_equal(x$cue[[field]], y$cue[[field]])
  }
  expect_equal(x$store$resources, y$store$resources)
  # Some elements should be different.
  for (field in c("string", "hash")) {
    expect_equal(length(y$command[[field]]), 1L)
    expect_false(x$command[[field]] == y$command[[field]])
  }
  expect_true("b" %in% x$command$deps)
  expect_false("f" %in% x$command$deps)
  expect_true(all(c("b", "f") %in% y$command$deps))
})

targets::tar_test("outer hook runs", {
  targets::tar_script({
    x <- targets::tar_target("a", "x1")
    tar_hook_outer(x, c(.x, "x2"))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), c("x1", "x2"))
})

targets::tar_test("outer hook can work with an empty command", {
  targets::tar_script({
    x <- targets::tar_target("a", NULL)
    tar_hook_outer(x, identity(.x))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), NULL)
})

targets::tar_test("outer hook can work with a symbol command", {
  targets::tar_script({
    y <- "y123"
    x <- targets::tar_target("a", y)
    tar_hook_outer(x, identity(.x))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), "y123")
})

targets::tar_test("outer hook invalidates target", {
  targets::tar_script({
    x <- targets::tar_target("a", "y")
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  targets::tar_script({
    x <- targets::tar_target("a", "y")
    tar_hook_outer(x, c(.x, "y2"))
  })
  expect_equal(targets::tar_outdated(callr_function = NULL), "a")
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_progress()
  expect_equal(out$name, "a")
  expect_equal(out$progress, "built")
})
