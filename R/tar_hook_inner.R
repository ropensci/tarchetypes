#' @title Hook to wrap dependencies
#' @export
#' @family hooks
#' @description In the command of each target, wrap each mention of
#'   each dependency target in an arbitrary R expression.
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
#'   The hook code is quoted (not evaluated) so there is no need
#'   to wrap it in `quote()`, `expression()`, or similar.
#' @param names_wrap Names of targets to wrap with the hook
#'   where they appear as dependencies in the commands of other targets.
#'   You can supply symbols, a character vector,
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
#' })
#' }
tar_hook_inner <- function(targets, hook, names = NULL, names_wrap = NULL) {
  targets <- tar_copy_targets(targets)
  hook <- substitute(hook)
  targets::tar_assert_lang(hook)
  assert_hook_placeholder(hook)
  names_quosure <- rlang::enquo(names)
  names_wrap_quosure <- rlang::enquo(names_wrap)
  env_wrap <- tar_hook_inner_env(
    targets = targets,
    hook = hook,
    names_wrap_quosure = names_wrap_quosure
  )
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_inner_insert,
    env_wrap = env_wrap
  )
  targets
}

tar_hook_inner_env <- function(targets, hook, names_wrap_quosure) {
  choices_wrap <- map_chr(
    unlist(list(targets), recursive = TRUE),
    ~.x$settings$name
  )
  names_wrap <- eval_tidyselect(names_wrap_quosure, choices_wrap) %|||%
    choices_wrap
  out <- map(names_wrap, ~tar_hook_inner_env_elt(name = .x, hook = hook))
  names(out) <- names_wrap
  out
}

tar_hook_inner_env_elt <- function(name, hook) {
  tar_sub_lang(hook, values = list(.x = as.symbol(name)))
}

tar_hook_inner_insert <- function(target, env_wrap) {
  assert_hook_expr(target)
  lang <- target$command$expr[[1]]
  expr <- tar_sub_expr(lang, values = env_wrap)
  tar_replace_command(target = target, expr = expr)
}
