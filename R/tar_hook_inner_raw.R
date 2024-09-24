#' @rdname tar_hook_inner
#' @export
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
