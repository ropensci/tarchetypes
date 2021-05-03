#' @title Hook to wrap code
#' @export
#' @family hooks
#' @description Wrap the command of each target in an arbitrary R expression.
#' @details The expression you supply to `hook`
#'   must contain the special placeholder symbol `.x`
#'   so `tar_hook_outer()` knows where to insert the original command
#'   of the target.
#' @return `NULL` (invisibly). The target objects are modified in place.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_hook_before Hooks
#' @inheritParams tar_hook_before
#' @param hook R code to wrap each target's command.
#'   The hook must contain the special placeholder symbol `.x`
#'   so `tar_hook_outer()` knows where to insert the original command
#'   of the target.
#'   The hook code is quoted (not evaluated) so there is no need
#'   to wrap it in `quote()`, `expression()`, or similar.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   targets <- list(
#'     # Nested target lists work with hooks.
#'     list(
#'       targets::tar_target(x1, task1()),
#'       targets::tar_target(x2, task2(x1))
#'     ),
#'     targets::tar_target(x3, task3(x2)),
#'     targets::tar_target(y1, task4(x3))
#'   )
#'   # Modifies target objects in place and invisibly returns NULL:
#'   tarchetypes::tar_hook_outer(
#'     targets = targets,
#'     hook = postprocess(.x, arg = "value"),
#'     names = starts_with("x")
#'   )
#'   targets # Return the target list.
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_outer <- function(targets, hook, names = NULL) {
  hook <- substitute(hook)
  assert_lang(hook)
  assert_hook_placeholder(hook)
  names_quosure <- rlang::enquo(names)
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_outer_insert,
    hook = hook
  )
  invisible()
}

tar_hook_outer_insert <- function(target, hook) {
  expr <- target$command$expr
  expr <- tar_sub_expr(hook, values = list(.x = call_brace(expr)))
  tar_replace_command(target = target, expr = expr)
}
