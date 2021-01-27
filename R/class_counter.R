# Internal counter class described at
# <https://books.ropensci.org/targets-design/classes.html#counter-class>.
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

counter_set_names <- function(counter, names) {
  lapply(names, counter_set_name, counter = counter)
}

counter_validate <- function(counter) {
  assert_correct_fields(counter, counter_new)
  assert_int(counter$count)
  assert_scalar(counter$count)
  assert_envir(counter$envir)
  if (length(names(counter$envir)) != counter$count) {
    throw_validate("envir does not match count.")
  }
}
