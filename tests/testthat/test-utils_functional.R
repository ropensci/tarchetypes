targets::tar_test("map()", {
  expect_equal(unname(map(letters, ~.x)), as.list(letters))
})

targets::tar_test("map2()", {
  expect_equal(
    as.numeric(map2(seq_len(4), seq_len(4), ~.x + .y)),
    seq_len(4) * 2
  )
})

targets::tar_test("map_chr()", {
  expect_equal(unname(map_chr(letters, ~.x)), letters)
})

targets::tar_test("map_int()", {
  expect_equal(unname(map_int(letters, ~length(.x))), rep(1, length(letters)))
})

targets::tar_test("map_lgl()", {
  out <- unname(map_lgl(seq_len(4L), ~ (.x > 2.5)))
  expect_equal(out, c(FALSE, FALSE, TRUE, TRUE))
})
