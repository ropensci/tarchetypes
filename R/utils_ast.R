#' @title Static code analysis for `tarchetypes`.
#' @export
#' @keywords internal
#' @description Walk an abstract syntax tree and capture data.
#' @details For internal use only. Not a user-side function.
#'   Powers functionality like automatic detection of `tar_load()`/`tar_read()`
#'   dependencies in [tar_render()].
#'   Packages `codetools` and `CodeDepends` have different (more sophisticated
#'   and elaborate) implementations of the concepts documented at
#'   <https://adv-r.hadley.nz/expressions.html#ast-funs>.
#' @return A character vector of data found during static code analysis.
#' @param expr A language object or function to scan.
#' @param walk_call A function to handle a specific kind of function call
#'   relevant to the code analysis at hand.
#' @examples
#' # How tar_render() really works:
#' expr <- quote({
#'   if (a > 1) {
#'     tar_load(target_name)
#'   }
#'   process_stuff(target_name)
#' })
#' walk_ast(expr, walk_call_knitr)
#' # Custom code analysis for developers of tarchetypes internals:
#' walk_custom <- function(expr, counter) {
#'   # New internals should use targets::tar_deparse_safe(backtick = FALSE).
#'   name <- deparse(expr[[1]])
#'   if (identical(name, "detect_this")) {
#'     counter_set_names(counter, as.character(expr[[2]]))
#'   }
#' }
#' expr <- quote({
#'   for (i in seq_len(10)) {
#'     for (j in seq_len(20)) {
#'       if (i > 1) {
#'         detect_this("prize")
#'       } else {
#'         ignore_this("penalty")
#'       }
#'     }
#'   }
#' })
#' walk_ast(expr, walk_custom)
walk_ast <- function(expr, walk_call) {
  counter <- counter_init()
  walk_expr(expr, counter, walk_call)
  sort(counter_get_names(counter))
}

walk_expr <- function(expr, counter, walk_call) {
  if (!length(expr)) {
    return()
  } else if (is.call(expr)) {
    walk_call(expr, counter)
    lapply(expr, walk_expr, counter = counter, walk_call = walk_call)
  } else if (typeof(expr) == "closure") {
    walk_expr(formals(expr), counter, walk_call)
    walk_expr(body(expr), counter, walk_call)
  } else if (is.pairlist(expr) || is.recursive(expr)) {
    lapply(expr, walk_expr, counter = counter, walk_call = walk_call)
  }
}
