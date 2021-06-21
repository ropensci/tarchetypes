#' @title Hook to prepend code
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @param targets A list of target objects. The input target list
#'   can be arbitrarily nested, but it must consist entirely of target
#'   objects. In addition, the return value is a simple list
#'   where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @param hook R code to insert. When you supply code to this argument,
#'   the code is quoted (not evaluated) so there is no need
#'   to wrap it in `quote()`, `expression()`, or similar.
#' @param names Name of targets in the target list
#'   to apply the hook. You can supply symbols, a character vector,
#'   or tidyselect helpers like [starts_with()].
#'   Targets not included in `names` still remain in the target list,
#'   but they are not modified because the hook does not apply to them.
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
#'   tarchetypes::tar_hook_before(
#'     targets = targets,
#'     hook = print("Running hook."),
#'     names = starts_with("x")
#'   )
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_before <- function(targets, hook, names = NULL) {
  targets <- tar_copy_targets(targets)
  hook <- substitute(hook)
  targets::tar_assert_lang(hook)
  names_quosure <- rlang::enquo(names)
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_before_insert,
    hook = hook
  )
  targets
}

tar_hook_before_insert <- function(target, hook) {
  assert_hook_expr(target)
  lang <- target$command$expr[[1]]
  expr <- as.expression(call_brace(list(hook, lang)))
  tar_replace_command(target = target, expr = expr)
}
