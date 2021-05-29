targets::tar_test("tar_select_names()", {
  targets <- list(
    list(
      targets::tar_target(x, 1),
      targets::tar_target(y1, 2)
    ),
    targets::tar_target(y2, 3),
    targets::tar_target(z, 4)
  )
  out <- tar_select_names(targets, starts_with("y"), contains("z"))
  expect_equal(sort(out), sort(c("y1", "y2", "z")))
})
