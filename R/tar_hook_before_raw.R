#' @title Hook to prepend code (raw version)
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets.
#'   Like [tar_hook_before()] except that the `hook` and `names`
#'   arguments are already pre-quoted language objects.
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
#' @param hook A pre-quoted language object with R code to insert.
#' @param names Name of targets in the target list
#'   to apply the hook. Supplied as a language object with a
#'   `tidyselect` expression: for example, `names = quote(starts_with("x_"))`.
#'   Targets not included in `names` still remain in the target list,
#'   but they are not modified because the hook does not apply to them.
#' @param set_deps Logical of length 1, whether to refresh the dependencies
#'   of each modified target by scanning the newly generated
#'   target commands for dependencies. If `FALSE`, then the target will
#'   keep the original set of dependencies it had before the hook.
#'   Set to `NULL` to include all targets supplied to the `targets` argument.
#'   `TRUE` is recommended for nearly all situations. Only use `FALSE`
#'   if you have a specialized use case and you know what you are doing.
#' @param envir Optional environment to construct the quosure for the `names`
#'   argument to select names.
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
#'   tarchetypes::tar_hook_before_raw(
#'     targets = targets,
#'     hook = quote(print("Running hook.")),
#'     names = quote(starts_with("x"))
#'   )
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_before_raw <- function(
  targets,
  hook,
  names = NULL,
  set_deps = TRUE,
  envir = parent.frame()
) {
  force(envir)
  targets::tar_assert_scalar(set_deps)
  targets::tar_assert_lgl(set_deps)
  targets::tar_assert_nonmissing(set_deps)
  targets <- tar_copy_targets(targets)
  targets::tar_assert_lang(hook)
  names_quosure <- rlang::as_quosure(names, env = envir)
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_before_insert,
    hook = hook,
    set_deps = set_deps
  )
  targets
}

tar_hook_before_insert <- function(target, hook, set_deps) {
  assert_hook_expr(target)
  lang <- target$command$expr[[1]]
  expr <- as.expression(call_brace(list(hook, lang)))
  tar_replace_command(target = target, expr = expr, set_deps = set_deps)
}
