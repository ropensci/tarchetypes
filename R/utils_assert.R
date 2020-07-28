assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    throw_validate(msg %||% "x must be a character.")
  }
}

assert_correct_fields <- function(object, constructor) {
  assert_identical_chr(sort(names(object)), sort(names(formals(constructor))))
}

assert_envir <- function(x, msg = NULL) {
  if (!is.environment(x)) {
    throw_validate(msg %||% "x must be an environment")
  }
}

assert_identical <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    throw_validate(msg %||% "x and y are not identical.")
  }
}

assert_identical_chr <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    msg_x <- paste0(deparse(x), collapse = "")
    msg_y <- paste0(deparse(y), collapse = "")
    throw_validate(msg %||% paste(msg_x, " and ", msg_y, " not identical."))
  }
}

assert_int <- function(x, msg = NULL) {
  if (!is.integer(x)) {
    throw_validate(msg %||% "x must be an integer vector.")
  }
}

assert_package <- function(package, msg = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    throw_validate(msg %||% paste("package ", package, " not installed"))
  }
}

assert_path <- function(path, msg = NULL) {
  missing <- !file.exists(path)
  if (any(missing)) {
    throw_validate(
      msg %||% paste(
        "missing files: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    throw_validate(msg %||% "x must have length 1.")
  }
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    throw_validate(paste(msg %||% "duplicated entries:", dups))
  }
}
