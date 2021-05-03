#' @title Hook to wrap dependencies
#' @export
#' @family hooks
#' @description In the command of each target, wrap each mention of
#'   each dependency target in an arbitrary R expression.
#' @details The expression you supply to `hook`
#'   must contain the special placeholder symbol `.x`
#'   so `tar_hook_inner()` knows where to insert the original command
#'   of the target.
#' @return `NULL` (invisibly). The target objects are modified in place.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_hook_before Hooks
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
#'   # Modifies target objects in place and invisibly returns NULL:
#'   tarchetypes::tar_hook_inner(
#'     targets = targets,
#'     hook = fun(.x),
#'     names = starts_with("x")
#'   )
#'   targets # Return the target list.
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_inner <- function(targets, hook, names = NULL, names_wrap = NULL) {
  hook <- substitute(hook)
  assert_lang(hook)
  assert_hook_placeholder(hook)
  names_quosure <- rlang::enquo(names)
  names_wrap_quosure <- rlang::enquo(names_wrap)
  env_wrap <- tar_hook_inner_env(
    hook = hook,
    names_wrap_quosure = names_wrap_quosure
  )
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_inner_insert,
    env_wrap = env_wrap
  )
  invisible()
}

tar_hook_inner_env <- function(hook, names_wrap_quosure) {
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
