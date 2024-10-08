#' @title Hook to wrap dependencies
#' @export
#' @family hooks
#' @description In the command of each target, wrap each mention of
#'   each dependency target in an arbitrary R expression.
#'
#'   `tar_hook_inner()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_inner_raw()` expects
#'   evaluated expression objects.
#' @details The expression you supply to `hook`
#'   must contain the special placeholder symbol `.x`
#'   so `tar_hook_inner()` knows where to insert the original command
#'   of the target.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_hook_before
#' @param hook R code to wrap each target's command.
#'   The hook must contain the special placeholder symbol `.x`
#'   so `tar_hook_inner()` knows where to insert the code to wrap
#'   mentions of dependencies.
#'
#'   `tar_hook_inner()` expects unevaluated expressions for the `hook` and
#'   `names` arguments, whereas `tar_hook_inner_raw()` expects
#'   evaluated expression objects.
#' @param names_wrap Names of targets to wrap with the hook
#'   where they appear as dependencies in the commands of other targets.
#'   Use `tidyselect` helpers like [starts_with()], as in
#'   `names_wrap = starts_with("your_prefix_")`.
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
#'     targets::tar_target(x3, task3(x2, x1)),
#'     targets::tar_target(y1, task4(x3))
#'   )
#'   tarchetypes::tar_hook_inner(
#'     targets = targets,
#'     hook = fun(.x),
#'     names = starts_with("x")
#'   )
#' })
#' targets::tar_manifest(fields = command)
#' # With tar_hook_inner_raw():
#' targets::tar_script({
#'   targets <- list(
#'     # Nested target lists work with hooks.
#'     list(
#'       targets::tar_target(x1, task1()),
#'       targets::tar_target(x2, task2(x1))
#'     ),
#'     targets::tar_target(x3, task3(x2, x1)),
#'     targets::tar_target(y1, task4(x3))
#'   )
#'   tarchetypes::tar_hook_inner_raw(
#'     targets = targets,
#'     hook = quote(fun(.x)),
#'     names = quote(starts_with("x"))
#'   )
#' })
#' })
#' }
tar_hook_inner <- function(
  targets,
  hook,
  names = NULL,
  names_wrap = NULL,
  set_deps = TRUE,
  envir = parent.frame()
) {
  force(envir)
  tar_hook_inner_raw(
    targets = targets,
    hook = substitute(hook),
    names = substitute(names),
    names_wrap = substitute(names_wrap),
    set_deps = set_deps,
    envir = envir
  )
}
