targets::tar_test("tar_tangle() manifest", {
  skip_if_not_installed("parsermd")
  tar_script(
    tarchetypes::tar_tangle(
      system.file(
        "example_tar_tangle.qmd",
        package = "tarchetypes",
        mustWork = TRUE
      )
    )
  )
  manifest <- targets::tar_manifest(
    callr_function = NULL,
    fields = tidyselect::everything(),
    drop_missing = FALSE
  )
  expect_equal(sort(manifest$name), sort(c("file", "data", "model")))
  expect_equal(manifest$format[manifest$name == "file"], "file")
  expect_equal(manifest$format[manifest$name == "model"], "qs")
  expect_true(grepl("^format_custom", manifest$format[manifest$name == "data"]))
  expect_equal(manifest$deployment[manifest$name == "model"], "worker")
  expect_equal(manifest$deployment[manifest$name != "model"], rep("main", 2L))
  expect_equal(manifest$storage[manifest$name == "data"], "worker")
  expect_equal(manifest$storage[manifest$name != "data"], rep("main", 2L))
})

targets::tar_test("tar_tangle() network", {
  skip_on_cran()
  skip_if_not_installed("parsermd")
  tar_script(
    tarchetypes::tar_tangle(
      system.file(
        "example_tar_tangle.qmd",
        package = "tarchetypes",
        mustWork = TRUE
      )
    )
  )
  network <- targets::tar_network()
  expect_equal(
    sort(network$vertices$name[network$vertices$type != "object"]),
    sort(c("file", "data", "model"))
  )
  edges <- network$edges
  expect_equal(nrow(edges), 2L)
  expect_equal(edges$to[edges$from == "file"], "data")
  expect_equal(edges$to[edges$from == "data"], "model")
})

targets::tar_test("tar_tangle() results", {
  skip_on_cran()
  skip_if_not_installed("parsermd")
  skip_if_not_installed("qs2")
  tar_script(
    tarchetypes::tar_tangle(
      system.file(
        "example_tar_tangle.qmd",
        package = "tarchetypes",
        mustWork = TRUE
      )
    )
  )
  utils::write.csv(airquality, "data.csv")
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(file), "data.csv")
  expect_true(is.data.frame(targets::tar_read(data)))
  expect_true(is.numeric(targets::tar_read(model)))
})
