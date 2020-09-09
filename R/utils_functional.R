map <- function(x, f, ...) {
  lapply(X = x, FUN = as_function(f), ...)
}

map_chr <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = character(1), ...)
}

map_int <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = integer(1), ...)
}
