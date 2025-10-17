#' @title Translate expression to pipeline
#' @export
#' @family Domain-specific languages for pipeline construction
#' @description Translate an ordinary non-`targets` imperative expression
#'   into a declarative list of targets for a pipeline.
#' @return A list of `tar_target()` objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param code An expression with ordinary R statements that assign
#'   values to variables.
#'   This code must use assignment operators `<-`, `=`, or `->` at the
#'   top level, but it need not call any functions in `targets`.
#'   See the example for details.
#' @param ... Named list of arguments to [targets::tar_target()]
#'   except for `name` and `command`.
#'   `tar_translate()` applies these arguments to all targets
#'   it generates from `code`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' write.csv(airquality, "data.csv", row.names = FALSE)
#' targets::tar_script({
#'   library(tarchetypes)
#'   tar_option_set(packages = c("readr", "dplyr", "ggplot2"))
#'   tar_translate({
#'     data <- airquality |>
#'       filter(!is.na(Ozone)) |>
#'       tar_target()
#'
#'     model = lm(Ozone ~ Temp, data) |>
#'       coefficients() |>
#'       tar_target()
#'
#'     plot <- ggplot(data) +
#'       geom_point(aes(x = Temp, y = Ozone)) +
#'       geom_abline(intercept = model[1], slope = model[2]) +
#'       theme_gray(24)
#'   }, memory = "transient")
#' })
#' targets::tar_make()
#' })
#' }
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
