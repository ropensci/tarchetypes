targets::tar_test("tar_hook_before() deep-copies the targets", {
  skip_on_cran()
  x <- targets::tar_target(x1, task1())
  y <- tar_hook_before(x, f())[[1]]
  y$cue$command <- FALSE
  y$settings$format <- "file"
  expect_equal(x$cue$command, TRUE)
  expect_equal(x$settings$format, "rds")
})

targets::tar_test("tar_hook_before() inserts code", {
  skip_on_cran()
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_before(
      targets = targets,
      hook = print("Running hook."),
      names = NULL
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(sort(out$name), sort(c("x1", "x2", "x3", "y1")))
  expect_true(all(grepl("Running hook", out$command)))
})

targets::tar_test("tar_hook_before() with tidyselect", {
  skip_on_cran()
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_before(
      targets = targets,
      hook = print("Running hook."),
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

targets::tar_test("tar_hook_before() changes internals properly", {
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
  y <- tar_hook_before(y, f())[[1]]
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
  expect_true("b" %in% (x$deps %|||% x$command$deps))
  expect_false("f" %in% (x$deps %|||% x$command$deps))
  expect_true(all(c("b", "f") %in% (y$deps %|||% y$command$deps)))
})

targets::tar_test("dep removed when global turns local", {
  skip_on_cran()
  x <- targets::tar_target("a", b)
  y <- targets::tar_target("a", b)
  y <- tar_hook_before(y, b <- 1)
  y$command$expr
  expect_true("b" %in% (x$deps %|||% x$command$deps))
  expect_false("b" %in% (y$deps %|||% y$command$deps))
})

targets::tar_test("hook runs", {
  skip_on_cran()
  targets::tar_script({
    x <- targets::tar_target("a", b)
    x
  })
  expect_error(targets::tar_make(callr_function = NULL))
  targets::tar_script({
    x <- targets::tar_target("a", b)
    tar_hook_before(x, b <- "y")
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), "y")
})

targets::tar_test("hook can work with an empty command", {
  skip_on_cran()
  targets::tar_script({
    x <- targets::tar_target("a", NULL)
    tar_hook_before(x, identity("x"))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), NULL)
})

targets::tar_test("hook can work with a symbol command", {
  skip_on_cran()
  targets::tar_script({
    y <- "y123"
    x <- targets::tar_target("a", y)
    tar_hook_before(x, identity("x"))
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(a), "y123")
})

targets::tar_test("hook invalidates target", {
  skip_on_cran()
  targets::tar_script({
    x <- targets::tar_target("a", "y")
    x
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  targets::tar_script({
    x <- targets::tar_target("a", "y")
    tar_hook_before(x, message("hook"))
  })
  expect_equal(targets::tar_outdated(callr_function = NULL), "a")
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_progress()
  expect_equal(out$name, "a")
  expect_equal(out$progress, status_completed())
})

targets::tar_test("tar_hook_before() sets deps by default", {
  skip_on_cran()
  x <- targets::tar_target(x1, task1())
  y <- tar_hook_before(x, f())[[1]]
  expect_true("task1" %in% (y$deps %|||% y$command$deps))
  expect_true("f" %in% (y$deps %|||% y$command$deps))
})

targets::tar_test("tar_hook_before() sets_deps = FALSE", {
  skip_on_cran()
  x <- targets::tar_target(x1, task1())
  y <- tar_hook_before(x, f(), set_deps = FALSE)[[1]]
  expect_true("task1" %in% (y$deps %|||% y$command$deps))
  expect_false("f" %in% (y$deps %|||% y$command$deps))
})

targets::tar_test("tar_hook_before(), set_deps = TRUE, deps arg", {
  skip_on_cran()
  targets::tar_script({
    tar_hook_before(
      targets = list(
        targets::tar_target_raw("upstream1", "I am upstream 1 running"),
        targets::tar_target_raw("upstream2", "I am upstream 2 running"),
        targets::tar_target_raw(
          name = "downstream",
          command = "I am downstream running",
          deps = c("upstream1", "upstream2")
        )
      ),
      hook = message("Running hook."),
      set_deps = TRUE
    )
  })
  edges <- targets::tar_network(callr_function = NULL)$edges
  expect_equal(nrow(edges), 0L)
})

targets::tar_test("tar_hook_before(), set_deps = FALSE, deps arg", {
  skip_on_cran()
  targets::tar_script({
    tar_hook_before(
      targets = list(
        targets::tar_target_raw("upstream1", "I am upstream 1 running"),
        targets::tar_target_raw("upstream2", "I am upstream 2 running"),
        targets::tar_target_raw(
          name = "downstream",
          command = "I am downstream running",
          deps = c("upstream1", "upstream2")
        )
      ),
      hook = message("Running hook."),
      set_deps = FALSE
    )
  })
  edges <- targets::tar_network(callr_function = NULL)$edges
  expect_equal(nrow(edges), 2L)
  expect_equal(edges$to, rep("downstream", 2L))
  expect_equal(sort(edges$from), sort(c("upstream1", "upstream2")))
})
