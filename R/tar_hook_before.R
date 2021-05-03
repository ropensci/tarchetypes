#' @title First hook
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets
#' @return A list of target objects.
#' @inheritSection tar_map Target objects
#' @param targets A list of target objects.
#' @param hook R code to insert. When you supply this argument,
#'   this code is quoted (not evaluated) so there is no need
#'   to wrap it in `quote()`, `expression()`, or similar.
#' @param names Name of targets in the target list
#'   to apply the hook. You can supply symbols, a character vector,
#'   or tidyselect helpers like [starts_with()].
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
  hook <- substitute(hook)
  assert_lang(hook)
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
  expr <- target$command$expr
  assert_nonmissing(expr[[1]], paste("target", name, "has no command."))
  expr[[1]] <- call_brace(list(hook, expr[[1]]))
  tar_replace_command(target = target, expr = expr)
}
