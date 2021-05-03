map <- function(x, f, ...) {
  lapply(X = x, FUN = as_function(f), ...)
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

walk_targets <- function(targets, names_quosure, fun, ...) {
  flat <- unlist(list(targets), recursive = TRUE)
  assert_targets(flat, "invalid target objects supplied to hook.")
  names <- map_chr(flat, ~.x$settings$name)
  names <- eval_tidyselect(names_quosure, names) %|||% names
  counter <- counter_init(names = names)
  recurse_targets(
    targets = targets,
    counter = counter,
    fun = fun,
    ...
  )
}

recurse_targets <- function(targets, counter, fun, ...) {
  if (is.list(targets) && !inherits(targets, "tar_target")) {
    lapply(
      targets,
      recurse_targets,
      counter = counter,
      fun = fun,
      ...
    )
    return()
  }
  is_hit <- inherits(targets, "tar_target") &&
    counter_exists_name(counter, targets$settings$name)
  if (is_hit) {
    fun(targets, ...)
  }
}
