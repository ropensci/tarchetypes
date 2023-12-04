targets::tar_test("tar_files() writes the correct targets", {
  targets::tar_script({
    list(
      tarchetypes::tar_files(x, c("a.txt", "b.txt"))
    )
  })
  out <- targets::tar_manifest(x, callr_function = NULL)
  expect_equal(out$name, "x")
  expect_equal(out$command, "x_files")
  expect_equal(out$pattern, "map(x_files)")
  out <- targets::tar_manifest(x_files, callr_function = NULL)
  expect_equal(out$name, "x_files")
  expect_equal(out$command, "c(\"a.txt\", \"b.txt\")")
  if ("pattern" %in% colnames(out)) {
    if (all(is.na(out$pattern))) {
      out$pattern <- NULL
    }
    expect_null(out$pattern)
  }
})

targets::tar_test("tar_files() correctly responds to changes in files", {
  file.create(c("a.txt", "b.txt"))
  targets::tar_script({
    list(
      tarchetypes::tar_files(x, c("a.txt", "b.txt"))
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
  expect_equal(sum(grepl(status_completed(), progress$progress)), 2L)
  writeLines("b", "b.txt")
  tar_make(callr_function = NULL)
  meta <- targets::tar_meta(starts_with("x_"))
  meta <- meta[meta$name != "x_files", ]
  expect_equal(nrow(meta), 2L)
  expect_equal(length(unique(meta$data)), 2L)
  progress <- targets::tar_progress(starts_with("x_"))
  progress <- progress[progress$name != "x_files", ]
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 1L)
  expect_equal(sum(grepl(status_completed(), progress$progress)), 1L)
})
