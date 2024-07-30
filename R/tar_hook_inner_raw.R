#' @title Hook to wrap dependencies (raw version)
#' @export
#' @family hooks
#' @description In the command of each target, wrap each mention of
#'   each dependency target in an arbitrary R expression.
#'   [tar_hook_inner_raw()] is like [tar_hook_inner()] except
#'   that the `hook`, `names`, and `names_wrap` arguments are
#'   pre-quoted language objects.
#' @details The expression you supply to `hook`
#'   must contain the special placeholder symbol `.x`
#'   so `tar_hook_inner()` knows where to insert the original command
#'   of the target.
#' @return A flattened list of target objects with the hooks applied.
#'   Even if the input target list had a nested structure,
#'   the return value is a simple list where each element is a target object.
#'   All hook functions remove the nested structure of the input target list.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_hook_before_raw
#' @param hook Pre-quoted language object with
#'   R code to wrap each target's command.
#'   The hook must contain the special placeholder symbol `.x`
#'   so `tar_hook_inner()` knows where to insert the code to wrap
#'   mentions of dependencies.
#' @param names_wrap Pre-quoted language object with
#'   names of targets to wrap with the hook
#'   where they appear as dependencies in the commands of other targets.
#'   Use `tidyselect` expressions, e.g.
#'   `names_wrap = quote(starts_with("your_prefix_"))`.
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
#'   tarchetypes::tar_hook_inner_raw(
#'     targets = targets,
#'     hook = quote(fun(.x)),
#'     names = quote(starts_with("x"))
#'   )
#' })
#' targets::tar_manifest(fields = command)
#' })
#' }
tar_hook_inner_raw <- function(
  targets,
  hook,
  names = NULL,
  names_wrap = NULL,
  set_deps = TRUE,
  envir = parent.frame()
) {
  force(envir)
  targets::tar_assert_scalar(set_deps)
  targets::tar_assert_lgl(set_deps)
  targets::tar_assert_nonmissing(set_deps)
  targets <- tar_copy_targets(targets)
  targets::tar_assert_lang(hook)
  assert_hook_placeholder(hook)
  names_quosure <- rlang::as_quosure(names, env = envir)
  names_wrap_quosure <- rlang::as_quosure(names_wrap, env = envir)
  env_wrap <- tar_hook_inner_env(
    targets = targets,
    hook = hook,
    names_wrap_quosure = names_wrap_quosure
  )
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_inner_insert,
    env_wrap = env_wrap,
    set_deps = set_deps
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

tar_hook_inner_insert <- function(target, env_wrap, set_deps) {
  assert_hook_expr(target)
  lang <- target$command$expr[[1]]
  expr <- tar_sub_expr(lang, values = env_wrap)
  tar_replace_command(target = target, expr = expr, set_deps = set_deps)
}
