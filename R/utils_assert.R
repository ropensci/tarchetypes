assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    throw_validate(msg %||% "x must be a character.")
  }
}

assert_correct_fields <- function(object, constructor) {
  assert_identical_chr(sort(names(object)), sort(names(formals(constructor))))
}

assert_dbl <- function(x, msg = NULL) {
  if (!is.numeric(x)) {
    throw_validate(msg %||% "x must be numeric.")
  }
}

assert_df <- function(x, msg = NULL) {
  if (!is.data.frame(x)) {
    throw_validate(msg %||% "x must be a data frame.")
  }
}

assert_equal_lengths <- function(x, msg = NULL) {
  lengths <- map_int(x, length)
  if (length(unique(lengths)) > 1L) {
    throw_validate(msg %||% "x must have equal-length elements.")
  }
}

assert_envir <- function(x, msg = NULL) {
  if (!is.environment(x)) {
    throw_validate(msg %||% "x must be an environment")
  }
}

assert_ge <- function(x, threshold, msg = NULL) {
  if (any(x < threshold)) {
    throw_validate(msg %||% paste("x is less than", threshold))
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

assert_in <- function(x, choices, msg = NULL) {
  if (!all(x %in% choices)) {
    msg <- msg %||% paste(
      deparse(substitute(x)),
      "equals",
      deparse(x),
      "but must be in",
      deparse(choices)
    )
    throw_validate(msg)
  }
}

assert_inherits <- function(x, class, msg = NULL) {
  if (!inherits(x, class)) {
    throw_validate(msg %||% paste("x does not inherit from", class))
  }
}

assert_int <- function(x, msg = NULL) {
  if (!is.integer(x)) {
    throw_validate(msg %||% "x must be an integer vector.")
  }
}

assert_lang <- function(x, msg = NULL) {
  if (!is.language(x)) {
    throw_validate(msg %||% "x must be a language object")
  }
}

assert_lgl <- function(x, msg = NULL) {
  if (!is.logical(x)) {
    throw_validate(msg %||% "x must be logical.")
  }
}

assert_list <- function(x, msg = NULL) {
  if (!is.list(x)) {
    throw_validate(msg %||% "x must be a list.")
  }
}

assert_names <- function(x, msg = NULL) {
  if (any(x != make.names(x, unique = FALSE))) {
    throw_validate(msg %||% "x must legal symbol names.")
  }
}

assert_nonempty <- function(x, msg = NULL) {
  if (length(x) < 1L) {
    throw_validate(msg %||% "x must be nonempty.")
  }
}

assert_not_expr <- function(x, msg = NULL) {
  if (is.expression(x)) {
    throw_validate(msg %||% "x must not be an expression object")
  }
}

assert_not_in <- function(x, choices, msg = NULL) {
  if (any(x %in% choices)) {
    throw_validate(msg %||% paste(deparse(x), " is in ", deparse(choices)))
  }
}

assert_nzchr <- function(x, msg = NULL) {
  if (any(!nzchar(x))) {
    throw_validate(msg %||% "x must not have empty strings.")
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

assert_targets <- function(x) {
  map(x, assert_inherits, class = "tar_target", msg = "... must have targets")
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    throw_validate(paste(msg %||% "duplicated entries:", dups))
  }
}

assert_values_list <- function(values) {
  assert_list(values, "values in tar_map() etc. must be a list or data frame.")
  assert_nonempty(names(values), "names(values) must not be empty.")
  assert_unique(names(values), "names(values) must be unique.")
  assert_chr(names(values), "names(values) must be a character.")
  assert_nzchr(names(values), "names(values) must not have empty strings.")
  assert_names(names(values), "names(values) must be legal symbol names.")
  assert_nonempty(values, "values in tar_map() must not be empty.")
  assert_equal_lengths(values, "values must have equal-length elements.")
}
