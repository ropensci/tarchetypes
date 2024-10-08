#' @title Hook to wrap commands
#' @export
#' @family hooks
#' @description Wrap the command of each target in an arbitrary R expression.
#'   `tar_hook_outer()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_outer_raw()` expects
#'   evaluated expression objects.
#' @details The expression you supply to `hook`
#'   must contain the special placeholder symbol `.x`
#'   so `tar_hook_outer()` knows where to insert the original command
#'   of the target.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_hook_before
#' @param hook R code to wrap each target's command.
#'   The hook must contain the special placeholder symbol `.x`
#'   so `tar_hook_outer()` knows where to insert the original command
#'   of the target.
#'
#'   `tar_hook_outer()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_outer_raw()` expects
#'   evaluated expression objects.
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
#'   tarchetypes::tar_hook_outer(
#'     targets = targets,
#'     hook = postprocess(.x, arg = "value"),
#'     names = starts_with("x")
#'   )
#' })
#' targets::tar_manifest(fields = command)
#' # Using tar_hook_outer_raw():
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
#'   tarchetypes::tar_hook_outer_raw(
#'     targets = targets,
#'     hook = quote(postprocess(.x, arg = "value")),
#'     names = quote(starts_with("x"))
#'   )
#' })
#' })
#' }
tar_hook_outer <- function(
  targets,
  hook,
  names = NULL,
  set_deps = TRUE,
  envir = parent.frame()
) {
  force(envir)
  tar_hook_outer_raw(
    targets = targets,
    hook = substitute(hook),
    names = substitute(names),
    set_deps = set_deps,
    envir = envir
  )
}
