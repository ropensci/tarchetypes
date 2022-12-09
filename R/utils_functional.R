map <- function(x, f, ...) {
  lapply(X = x, FUN = as_function(f), ...)
}

map2 <- function(x, y, f, ...) {
  targets::tar_assert_equal_lengths(x, y)
  fun <- as_function(f)
  out <- list()
  for (i in seq_along(x)) {
    out[[i]] <- fun(.x = x[[i]], .y = y[[i]], ...)
  }
  out
}

map_chr <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = character(1), ...)
}

map_int <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = integer(1), ...)
}

map_lgl <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = logical(1), ...)
}

map_rows <- function(x, f, ...) {
  apply(X = x, MARGIN = 1L, FUN = as_function(f), ...)
}

set_names <- function(x, names) {
  names(x) <- unname(names)
  x
}

transpose <- function(x) {
  x <- lapply(x, as.list)
  lapply(seq_along(x[[1]]), transpose_elt, x = x)
}

transpose_elt <- function(index, x) {
  lapply(x, `[[`, index)
}
