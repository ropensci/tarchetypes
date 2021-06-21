#' @title Counter constructor.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @details Creates a counter object as described at
#'   <https://books.ropensci.org/targets-design/classes.html#counter-class>.
#' @return A new counter object.
#' @param names Character vector of names to add to the new counter.
#' @examples
#' counter <- counter_init()
#' counter_set_names(counter, letters)
counter_init <- function(names = NULL) {
  count <- length(names)
  envir <- new.env(hash = TRUE, parent = emptyenv())
  lapply(names, assign, value = TRUE, envir = envir, inherits = FALSE)
  counter_new(count = count, envir = envir)
}

counter_new <- function(count = NULL, envir = NULL) {
  force(count)
  force(envir)
  environment()
}

counter_get_names <- function(counter) {
  names(counter$envir)
}

counter_exists_name <- function(counter, name) {
  exists(name, envir = counter$envir, inherits = FALSE)
}

counter_set_name <- function(counter, name) {
  is_new <- !counter_exists_name(counter, name)
  assign(x = name, value = TRUE, envir = counter$envir)
  counter$count <- counter$count + is_new
}

#' @title Add data to an existing counter object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return `NULL` (invisibly)
#' @param counter A counter object, defined for internal purposes only.
#' @param names Character vector of names to add to the counter.
#' @examples
#' counter <- counter_init()
#' counter_set_names(counter, letters)
counter_set_names <- function(counter, names) {
  lapply(names, counter_set_name, counter = counter)
  invisible()
}

counter_validate <- function(counter) {
  targets::tar_assert_int(counter$count)
  targets::tar_assert_scalar(counter$count)
  targets::tar_assert_envir(counter$envir)
  if (length(names(counter$envir)) != counter$count) {
    targets::tar_throw_validate("envir does not match count.")
  }
}
