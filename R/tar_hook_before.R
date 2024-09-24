#' @title Hook to prepend code
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets.
#'   `tar_hook_before()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_before_raw()` expects
#'   evaluated expression objects.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @param hook R code to insert.
#'   `tar_hook_before()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_before_raw()` expects
#'   evaluated expression objects.
#' @param names Name of targets in the target list
#'   to apply the hook. Supplied using `tidyselect` helpers like
#'   [starts_with()], as in `names = starts_with("your_prefix_")`.
#'   Set to `NULL` to include all targets supplied to the `targets` argument.
#'   Targets not included in `names` still remain in the target list,
#'   but they are not modified because the hook does not apply to them.
#'
#'   The regular hook functions expects unevaluated expressions for the `hook`
#'   and `names` arguments, whereas the `"_raw"` versions expect
#'   evaluated expression objects.
#' @param set_deps Logical of length 1, whether to refresh the dependencies
#'   of each modified target by scanning the newly generated
#'   target commands for dependencies. If `FALSE`, then the target will
#'   keep the original set of dependencies it had before the hook.
#'   Set to `NULL` to include all targets supplied to the `targets` argument.
#'   `TRUE` is recommended for nearly all situations. Only use `FALSE`
#'   if you have a specialized use case and you know what you are doing.
#' @param envir Optional environment to construct the quosure for the `names`
#'   argument to select names.
#' @param targets A list of target objects. The input target list
#'   can be arbitrarily nested, but it must consist entirely of target
#'   objects. In addition, the return value is a simple list
#'   where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
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
#' # With tar_hook_before_raw():
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
#' }
tar_hook_before <- function(
  targets,
  hook,
  names = NULL,
  set_deps = TRUE,
  envir = parent.frame()
) {
  force(envir)
  tar_hook_before_raw(
    targets = targets,
    hook = substitute(hook),
    names = substitute(names),
    set_deps = set_deps,
    envir = envir
  )
}
