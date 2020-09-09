tar_test("map_chr()", {
  expect_equal(unname(map_chr(letters, ~.x)), letters)
})

tar_test("map_int()", {
  expect_equal(unname(map_int(letters, ~length(.x))), rep(1, length(letters)))
})
