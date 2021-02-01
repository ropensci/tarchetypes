expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

skip_pandoc <- function() {
  has_pandoc <- rmarkdown::pandoc_available(version = "1.12.3", error = FALSE)
  skip_if_not(has_pandoc, "no pandoc >= 1.12.3")
}
