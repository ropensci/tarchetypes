expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

skip_quarto <- function() {
  skip_pandoc()
  skip_if_not_installed("quarto")
  if (is.null(quarto::quarto_path())) {
    skip("Quarto not found.")
  }
}

skip_rmarkdown <- function() {
  skip_pandoc()
  skip_if_not_installed("rmarkdown")
}

skip_pandoc <- function() {
  has_pandoc <- rmarkdown::pandoc_available(version = "1.12.3", error = FALSE)
  skip_if_not(has_pandoc, "no pandoc >= 1.12.3")
}

targets_1.3.2.9004 <- function() {
  utils::compareVersion(
    a = as.character(packageVersion("targets")),
    b = "1.3.2.9004"
  ) > -1L
}

status_completed <- function() {
  if (targets_1.3.2.9004()) {
    "completed"
  } else {
    "built"
  }
}

status_dispatched <- function() {
  if (targets_1.3.2.9004()) {
    "dispatched"
  } else {
    "started"
  }
}
