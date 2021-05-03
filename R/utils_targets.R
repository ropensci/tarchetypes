tar_copy_targets <- function(targets) {
  targets <- unlist(list(targets), recursive = TRUE)
  msg <- "expected a target list but found non-target objects."
  assert_targets(targets, msg)
  lapply(targets, tar_copy_target)
}

tar_copy_target <- function(target) {
  targets::tar_target_raw(
    name = target$settings$name,
    command = target$command$expr,
    pattern = target$settings$pattern,
    packages = target$command$packages,
    library = target$command$library,
    format = target$settings$format,
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
    )
  )
}

tar_replace_command <- function(target, expr) {
  pilot <- targets::tar_target_raw(
    name = target$settings$name,
    command = expr,
    packages = target$command$packages,
    library = target$command$library,
    pattern = target$settings$pattern
  )
  target$command <- pilot$command
  invisible()
}
