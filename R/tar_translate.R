#' @export
#' @family Domain-specific languages for pipeline construction
tar_translate <- function(code, ...) {
  statements <- tar_translate_statements(match.call()$code)
  envir <- targets::tar_option_get("envir")
  settings <- list(...)
  tar_translate_assert_settings(settings)
  lapply(statements, tar_translate_target, envir = envir, settings = settings)
}

tar_translate_target <- function(statement, envir, settings) {
  message <- paste(
    "failed to parse a statement because it was not",
    "a true assignemnt statement using =, <-, or ->."
  )
  targets::tar_assert_true(length(statement) > 2L, msg = message)
  operator <- as.character(statement[[1L]])
  targets::tar_assert_in(operator, c("=", "<-", "->"), msg = message)
  if (identical(operator, "->")) {
    settings$name <- statement[[3L]]
    settings$command <- statement[[2L]]
  } else {
    settings$name <- statement[[2L]]
    settings$command <- statement[[3L]]
  }
  do.call(what = targets::tar_target, args = settings)
}

tar_translate_statements <- function(expresison) {
  statements <- if_any(
    identical(expresison[[1L]], quote(`{`)),
    as.list(expresison[-1L]),
    list(expresison)
  )
  tar_translate_assert_statements(statements)
  statements
}

tar_translate_assert_statements <- function(statements) {
  targets::tar_assert_true(
    all(map_lgl(statements, ~ rlang::is_call(.x, c("<-", "=")))),
    msg = paste(
      "code argument must be enclosed in curly braces if",
      "it has multiple statements, and each statement",
      "must be an assignment statement using the left arrow <-",
      "or equal sign ="
    )
  )
}

tar_translate_assert_settings <- function(settings) {
  targets:::tar_assert_named(
    settings,
    msg = "ellipsis arguments must all be named."
  )
}
