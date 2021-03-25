#' @title Create multiple expressions with symbol substitution (raw version).
#' @export
#' @description Loop over a grid of values and create an expression object
#'   from each one. Helps with general metaprogramming. Unlike [tar_sub()],
#'   which quotes the `expr` argument, `tar_sub_raw()` assumes `expr`
#'   is an expression object.
#' @return A list of expression objects. Often, these expression objects
#'   evaluate to target objects (but not necessarily).
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
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
  assert_lang(expr)
  assert_values_list(values)
  lapply(transpose(values), tar_sub_expr, expr = expr)
}

tar_sub_expr <- function(expr, values) {
  lang <- substitute(
    substitute(expr = expr, env = env),
    env = list(expr = expr, env = values)
  )
  as.expression(eval(lang))
}
