#' @title Create multiple expressions with symbol substitution (raw version).
#' @export
#' @family Metaprogramming utilities
#' @description Loop over a grid of values and create an expression object
#'   from each one. Helps with general metaprogramming. Unlike [tar_sub()],
#'   which quotes the `expr` argument, `tar_sub_raw()` assumes `expr`
#'   is an expression object.
#' @return A list of expression objects. Often, these expression objects
#'   evaluate to target objects (but not necessarily).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param expr Expression object with the starting expression.
#'   Values are iteratively substituted
#'   in place of symbols in `expr` to create each new expression.
#' @param values List of values to substitute into `expr` to create
#'   the expressions. All elements of `values` must have the same length.
#' @examples
#' # tar_map() is incompatible with tar_render() because the latter
#' # operates on preexisting tar_target() objects. By contrast,
#' # tar_eval_raw() and tar_sub_raw() iterate over code farther upstream.
#' values <- list(
#'   name = lapply(c("name1", "name2"), as.symbol),
#'   file = c("file1.Rmd", "file2.Rmd")
#' )
#' tar_sub_raw(quote(tar_render(name, file)), values = values)
tar_sub_raw <- function(expr, values) {
  targets::tar_assert_lang(expr)
  assert_values_list(values)
  lapply(transpose(values), tar_sub_expr, expr = expr)
}

tar_sub_expr <- function(expr, values) {
  as.expression(tar_sub_lang(expr, values))
}

tar_sub_lang <- function(expr, values) {
  out <- substitute(
    substitute(expr = expr, env = env),
    env = list(expr = expr, env = values)
  )
  eval(out)
}
