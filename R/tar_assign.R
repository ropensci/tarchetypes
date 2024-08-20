#' @title An assignment-based pipeline DSL
#' @export
#' @family Domain-specific languages for pipeline construction
#' @description An assignment-based domain-specific language for pipeline
#'   construction.
#' @return A list of `tar_target()` objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param targets An expression with special syntax to define a
#'   collection of targets in a pipeline.
#'   Example: `tar_assign(x <- tar_target(get_data()))` is equivalent
#'   to `list(tar_target(x, get_data()))`.
#'   The rules of the syntax
#'   are as follows:
#'   * The code supplied to [tar_assign()] must be enclosed in curly braces
#'     beginning with `{` and `}` unless it only contains a
#'     one-line statement or uses `=` as the assignment.
#'   * Each statement in the code block must be of the form
#'     `x <- f()`, or `x = f()` where `x` is the name of a target and
#'     `f()` is a function like `tar_target()` or [tar_quarto()]
#'     which accepts a `name` argument.
#'   * The native pipe operator `|>` is allowed because it lazily
#'     evaluates its arguments and be converted into non-pipe syntax
#'     without evaluating the code.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' write.csv(airquality, "data.csv", row.names = FALSE)
#' targets::tar_script({
#'   library(tarchetypes)
#'   tar_option_set(packages = c("readr", "dplyr", "ggplot2"))
#'   tar_assign({
#'     file <- tar_target("data.csv", format = "file")
#'
#'     data <- read_csv(file, col_types = cols()) |>
#'       filter(!is.na(Ozone)) |>
#'       tar_target()
#'
#'     model = lm(Ozone ~ Temp, data) |>
#'       coefficients() |>
#'       tar_target()
#'
#'     plot <- {
#'         ggplot(data) +
#'           geom_point(aes(x = Temp, y = Ozone)) +
#'           geom_abline(intercept = model[1], slope = model[2]) +
#'           theme_gray(24)
#'       } |>
#'         tar_target()
#'   })
#' })
#' targets::tar_make()
#' })
#' }
tar_assign <- function(targets) {
  expr <- match.call()$targets
  statements <- if_any(
    identical(expr[[1L]], quote(`{`)),
    as.list(expr[-1L]),
    list(expr)
  )
  targets::tar_assert_true(
    all(map_lgl(statements, ~rlang::is_call(.x, c("<-", "=")))),
    msg = paste(
      "tar_assign() code must be enclosed in curly braces if",
      "it has multiple statements, and each statement",
      "must be an assignment statement using the left arrow <-",
      "or equal sign ="
    )
  )
  envir <- targets::tar_option_get("envir")
  lapply(statements, tar_assign_parse, envir = envir)
}

tar_assign_parse <- function(statement, envir) {
  targets::tar_assert_true(
    length(statement) > 2L && is.call(statement[[3L]]),
    msg = paste0(
      "tar_assign() failed to parse statement `",
      tar_deparse_language(statement),
      "` because the right-hand side is not a function call."
    )
  )
  call <- statement[[3L]]
  call$name <- statement[[2L]]
  eval(call, envir = envir)
}
