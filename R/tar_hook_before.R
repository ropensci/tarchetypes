#' @title Hook to prepend code
#' @export
#' @family hooks
#' @description Prepend R code to the commands of multiple targets.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_hook_before_raw
#' @param hook R code to insert. When you supply code to this argument,
#'   the code is quoted (not evaluated) so there is no need
#'   to wrap it in `quote()`, `expression()`, or similar.
#' @param names Name of targets in the target list
#'   to apply the hook. Supplied with `tidyselect` helpers like
#'   [starts_with()], as in `names = starts_with("your_prefix_")`.
#'   Set to `NULL` to include all targets supplied to the `targets` argument.
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
