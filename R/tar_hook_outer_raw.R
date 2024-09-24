#' @rdname tar_hook_outer
#' @export
tar_hook_outer_raw <- function(
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
  assert_hook_placeholder(hook)
  names_quosure <- rlang::as_quosure(names, env = envir)
  walk_targets(
    targets = targets,
    names_quosure = names_quosure,
    fun = tar_hook_outer_insert,
    hook = hook,
    set_deps = set_deps
  )
  targets
}

tar_hook_outer_insert <- function(target, hook, set_deps) {
  assert_hook_expr(target)
  lang <- target$command$expr[[1]]
  expr <- tar_sub_expr(hook, values = list(.x = lang))
  tar_replace_command(target = target, expr = expr, set_deps = set_deps)
}
