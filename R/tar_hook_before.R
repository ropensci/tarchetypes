#' @title Hook to prepend code
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets.
#' @return `NULL` (invisibly). The target objects are modified in place.
#' @inheritSection tar_map Target objects
#' @section Hooks:
#'   Hook functions make in-place modifications to
#'   the commands of target objects and invisibly return `NULL`.
#'   Users are responsible for fully populating the final target list
#'   at the end of `_targets.R`.
#' @param targets A list of target objects.
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
#'   # Modifies target objects in place and invisibly returns NULL:
#'   tarchetypes::tar_hook_before(
#'     targets = targets,
#'     hook = print("Running hook."),
#'     names = starts_with("x")
#'   )
#'   targets # Return the target list.
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_before <- function(targets, hook, names = NULL) {
  hook <- substitute(hook)
  assert_lang(hook)
  names_quosure <- rlang::enquo(names)
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_before_insert,
    hook = hook
  )
  invisible()
}

tar_hook_before_insert <- function(target, hook) {
  expr <- target$command$expr
  expr <- as.expression(call_brace(list(hook, call_brace(expr))))
  tar_replace_command(target = target, expr = expr)
}
