targets::tar_test("tar_hook_inner() deep-copies the targets", {
  x <- targets::tar_target(x1, task1())
  y <- tar_hook_inner(x, f(.x))[[1]]
  y$cue$command <- FALSE
  y$settings$format <- "file"
  expect_equal(x$cue$command, TRUE)
  expect_equal(x$settings$format, "rds")
})

targets::tar_test("tar_hook_inner() requires .x", {
  x <- tar_target(x, 1)
  expect_error(
    tar_hook_inner(x, f()),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_hook_inner() inserts code", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_inner(
      targets = targets,
      hook = f(.x, "Running hook."),
      names = NULL
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_true(all(grepl("Running hook", out$command[out$name != "x1"])))
})

targets::tar_test("tar_hook_inner() with tidyselect", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_inner(
      targets = targets,
      hook = f(.x, "Running hook."),
      names = tidyselect::starts_with("x")
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_equal(
    grepl("Running hook", out$command),
    grepl("^x", out$name) & out$name != "x1"
  )
})

targets::tar_test("tar_hook_inner() with tidyselect on names_wrap", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_inner(
      targets = targets,
      hook = f(.x, "Running hook."),
      names_wrap = tidyselect::all_of(c("x2", "x3"))
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_equal(
    grepl("Running hook", out$command),
    grepl("x3|y1", out$name)
  )
})

targets::tar_test("tar_hook_inner() with no replacement", {
  x <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = list(x = 1)
  )
  y <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = list(x = 1)
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
  z <- tar_hook_inner(y, f(.x))[[1]]
  for (field in c("packages", "library", "deps", "seed", "string", "hash")) {
    expect_equal(x$command[[field]], z$command[[field]])
  }
  for (field in setdiff(names(x$settings), "pattern")) {
    expect_equal(x$settings[[field]], z$settings[[field]])
  }
  expect_equal(deparse(x$settings$pattern), deparse(z$settings$pattern))
  for (field in names(x$cue)) {
    expect_equal(x$cue[[field]], z$cue[[field]])
  }
  expect_equal(x$store$resources, z$store$resources)
})

targets::tar_test("tar_hook_inner() changes internals properly", {
  x <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = list(x = 1)
  )
  y <- targets::tar_target(
    "a",
    b,
    pattern = map(c),
    format = "file",
    resources = list(x = 1)
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
  y <- tar_hook_inner(list(y, tar_target(b, 1)), f(.x))[[1]]
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

targets::tar_test("inner hook runs", {
  targets::tar_script({
    x <- list(
      targets::tar_target(a, "x1"),
      targets::tar_target(b, a)
    )
    tar_hook_inner(x, c(.x, "x2"))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(b), c("x1", "x2"))
})

targets::tar_test("inner hook can work with an empty command", {
  targets::tar_script({
    x <- targets::tar_target("a", NULL)
    tar_hook_inner(x, identity(.x))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), NULL)
})

targets::tar_test("inner hook invalidates target", {
  targets::tar_script({
    x <- list(
      targets::tar_target(a, "x1"),
      targets::tar_target(b, a)
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  targets::tar_script({
    x <- list(
      targets::tar_target(a, "x1"),
      targets::tar_target(b, a)
    )
    tar_hook_inner(x, c(.x, "y2"))
  })
  expect_equal(targets::tar_outdated(callr_function = NULL), "b")
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_progress()
  expect_equal(out$name, "b")
  expect_equal(out$progress, "built")
})
