targets::tar_test("tar_select_targets()", {
  targets <- list(
    list(
      targets::tar_target(x, 1),
      targets::tar_target(y1, 2)
    ),
    targets::tar_target(y2, 3),
    targets::tar_target(z, 4)
  )
  out <- tar_select_targets(targets, starts_with("y"), contains("z"))
  expect_equal(sort(names(out)), sort(c("y1", "y2", "z")))
  expect_equal(length(out), 3L)
  for (name in c("y1", "y2", "z")) {
    expect_equal(out[[name]]$settings$name, name)
  }
})
