#' @title Create multiple expressions with symbol substitution.
#' @export
#' @description Loop over a grid of values and create an expression object
#'   from each one. Helps with general metaprogramming.
#' @return A list of expression objects. Often, these expression objects
#'   evaluate to target objects (but not necessarily).
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_sub_raw
#' @param expr Starting expression. Values are iteratively substituted
#'   in place of symbols in `expr` to create each new expression.
#' @examples
#' # tar_map() is incompatible with tar_render() because the latter
#' # operates on preexisting tar_target() objects. By contrast,
#' # tar_eval() and tar_sub() iterate over code farther upstream.
#' values <- list(
#'   name = lapply(c("name1", "name2"), as.symbol),
#'   file = list("file1.Rmd", "file2.Rmd")
#' )
#' tar_sub(tar_render(name, file), values = values)
tar_sub <- function(expr, values) {
  tar_sub_raw(expr = substitute(expr), values = values)
}
