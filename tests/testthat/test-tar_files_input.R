tar_test("tar_files_input() writes the correct targets", {
  targets::tar_script({
    tar_pipeline(
      tarchetypes::tar_files_input(x, c("a.txt", "b.txt"))
    )
  })
  out <- targets::tar_manifest(x, callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "x_files")
  expect_equal(out$pattern, "map(x_files)")
  out <- targets::tar_manifest(x_files, callr_function = NULL)
  expect_equal(out$name, "x_files")
  expect_equal(out$command, "list(\"a.txt\", \"b.txt\")")
  expect_equal(out$pattern, NA_character_)
})

tar_test("tar_files_input() writes the correct targets (batched)", {
  targets::tar_script({
    tar_pipeline(
      tarchetypes::tar_files_input(
        x,
        files = c("a.txt", "b.txt", "c.txt", "d.txt"),
        batches = 2
      )
    )
  })
  out <- targets::tar_manifest(x, callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "x_files")
  expect_equal(out$pattern, "map(x_files)")
  out <- targets::tar_manifest(x_files, callr_function = NULL)
  expect_equal(out$name, "x_files")
  expect_equal(
    out$command,
    "list(c(\"a.txt\", \"b.txt\"), c(\"c.txt\", \"d.txt\"))"
  )
  expect_equal(out$pattern, NA_character_)
})

tar_test("tar_files_input() correctly responds to changes in files", {
  file.create(c("a.txt", "b.txt", "c.txt", "d.txt"))
  targets::tar_script({
    tar_pipeline(
      tarchetypes::tar_files_input(
        x,
        files = c("a.txt", "b.txt", "c.txt", "d.txt"),
        batches = 2
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  # If we change one file, `tar_make()` will only rerun one branch.
  meta <- targets::tar_meta(starts_with("x_"))
  meta <- meta[meta$name != "x_files", ]
  expect_equal(nrow(meta), 2L)
  expect_equal(length(unique(meta$data)), 1L)
  progress <- targets::tar_progress(starts_with("x_"))
  progress <- progress[progress$name != "x_files", ]
  expect_equal(nrow(progress), 2L)
  expect_equal(sum(grepl("built", progress$progress)), 2L)
  writeLines("b", "b.txt")
  tar_make(callr_function = NULL)
  meta <- targets::tar_meta(starts_with("x_"))
  meta <- meta[meta$name != "x_files", ]
  expect_equal(nrow(meta), 2L)
  expect_equal(length(unique(meta$data)), 2L)
  progress <- targets::tar_progress(starts_with("x_"))
  progress <- progress[progress$name != "x_files", ]
  expect_equal(nrow(progress), 1L)
  expect_equal(sum(grepl("built", progress$progress)), 1L)
  # results
  out <- targets::tar_read(x_files)
  exp <- list(
    c("a.txt", "b.txt"),
    c("c.txt", "d.txt")
  )
  expect_equal(out, exp)
  out <- targets::tar_read(x, branches = 1)
  expect_equal(out, c("a.txt", "b.txt"))
  out <- targets::tar_read(x, branches = 2)
  expect_equal(out, c("c.txt", "d.txt"))
})