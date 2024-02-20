tar_copy_targets <- function(targets) {
  targets <- unlist(list(targets), recursive = TRUE)
  targets::tar_assert_target_list(targets)
  lapply(targets, tar_copy_target)
}

tar_copy_target <- function(target) {
  targets::tar_target_raw(
    name = target$settings$name,
    command = target$command$expr,
    pattern = target$settings$pattern,
    packages = target$command$packages,
    library = target$command$library,
    deps = target$command$deps,
    string = target$command$string,
    format = target$settings$format,
    repository = target$settings$repository,
    iteration = target$settings$iteration,
    error = target$settings$error,
    memory = target$settings$memory,
    garbage_collection = target$settings$garbage_collection,
    deployment = target$settings$deployment,
    priority = target$settings$priority,
    resources = target$settings$resources,
    storage = target$settings$storage,
    retrieval = target$settings$retrieval,
    cue = targets::tar_cue(
      mode = target$cue$mode,
      command = target$cue$command,
      depend = target$cue$depend,
      format = target$cue$format,
      iteration = target$cue$iteration,
      file = target$cue$file
    ),
    description = target$settings$description
  )
}

tar_replace_command <- function(target, expr, set_deps) {
  pilot <- targets::tar_target_raw(
    name = target$settings$name,
    command = expr,
    packages = target$command$packages,
    library = target$command$library,
    pattern = target$settings$pattern,
    deps = if_any(set_deps, NULL, target$command$deps)
  )
  target$command <- pilot$command
  invisible()
}

walk_targets <- function(targets, names_quosure, fun, ...) {
  flat <- unlist(list(targets), recursive = TRUE)
  targets::tar_assert_target_list(flat)
  names <- map_chr(flat, ~.x$settings$name)
  names <- eval_tidyselect(names_quosure, names) %|||% names
  counter <- counter_init(names = names)
  recurse_targets(
    targets = targets,
    counter = counter,
    fun = fun,
    ...
  )
}

recurse_targets <- function(targets, counter, fun, ...) {
  if (is.list(targets) && !inherits(targets, "tar_target")) {
    lapply(
      targets,
      recurse_targets,
      counter = counter,
      fun = fun,
      ...
    )
    return()
  }
  is_hit <- inherits(targets, "tar_target") &&
    counter_exists_name(counter, targets$settings$name)
  if (is_hit) {
    fun(targets, ...)
  }
}
