#' @title Create multiple expressions with symbol substitution.
#' @export
#' @family Metaprogramming utilities
#' @description Loop over a grid of values and create an expression object
#'   from each one. Helps with general metaprogramming.
#'
#'   [tar_sub()] expects an unevaluated expression for
#'   the `expr` object, whereas [tar_sub_raw()] expects an
#'   evaluated expression object.
#' @return A list of expression objects. Often, these expression objects
#'   evaluate to target objects (but not necessarily).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param expr Starting expression. Values are iteratively substituted
#'   in place of symbols in `expr` to create each new expression.
#'
#'   [tar_sub()] expects an unevaluated expression for
#'   the `expr` object, whereas [tar_sub_raw()] expects an
#'   evaluated expression object.
#' @param values List of values to substitute into `expr` to create
#'   the expressions. All elements of `values` must have the same length.
#' @examples
#' # tar_map() is incompatible with tar_render() because the latter
#' # operates on preexisting tar_target() objects. By contrast,
#' # tar_eval() and tar_sub() iterate over code farther upstream.
#' values <- list(
#'   name = lapply(c("name1", "name2"), as.symbol),
#'   file = list("file1.Rmd", "file2.Rmd")
#' )
#' tar_sub(tar_render(name, file), values = values)
#' tar_sub_raw(quote(tar_render(name, file)), values = values)
tar_sub <- function(expr, values) {
  tar_sub_raw(expr = substitute(expr), values = values)
}
