#' @title Static branching.
#' @export
#' @description Define collections of new targets.
#' @details `tar_map()` creates collections of new
#'   targets by iterating over a list of arguments
#'   and substituting symbols into commands and pattern statements.
#' @return A list of new target objects. If `unlist` is `FALSE`,
#'   the list is nested and sub-lists are named and grouped by the original
#'   input targets. If `unlist = TRUE`, the return value is a flat list of
#'   targets named by the new target names.
#' @param values Named list or data frame with values to iterate over.
#'   The names are the names of symbols in the commands and pattern
#'   statements, and the elements are values that get substituted
#'   in place of those symbols. Elements of the `values` list
#'   should be small objects that can easily deparse to names,
#'   such as characters, integers, and symbols.
#'   To create a list of symbols as a column of `values`,
#'   use `rlang::syms()`.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `targets::tar_pipeline()`.
#' @param names Subset of `names(values)`
#'   used to generate the suffixes in the names of the new targets.
#'   You can supply symbols, a character vector,
#'   or tidyselect helpers like [starts_with()].
#' @param unlist Logical, whether to flatten the returned list of targets.
#'   If `unlist = FALSE`, the list is nested and sub-lists
#'   are named and grouped by the original input targets.
#'   If `unlist = TRUE`, the return value is a flat list of targets
#'   named by the new target names.
#' @examples
#' targets::tar_dir({
#' targets::tar_script({
#'   targets::tar_pipeline(
#'     tarchetypes::tar_map(
#'       list(a = c(12, 34), b = c(45, 78)),
#'       targets::tar_target(x, a + b),
#'       targets::tar_target(y, x + a, pattern = map(x))
#'     )
#'   )
#' })
#' targets::tar_manifest(callr_function = NULL)
#' })
tar_map <- function(
  values,
  ...,
  names = tidyselect::everything(),
  unlist = FALSE
) {
  targets <- unlist(list(...), recursive = TRUE)
  assert_targets(targets)
  tar_map_assert_values(values)
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, base::names(values))
  values <- tar_map_extend_values(targets, values, names)
  out <- lapply(targets, tar_map_target, values = values)
  flat <- unlist(out, recursive = TRUE)
  trn(
    unlist,
    set_names(flat, map_chr(flat, ~.x$settings$name)),
    set_names(out, map_chr(targets, ~.x$settings$name))
  )
}

tar_map_assert_values <- function(values) {
  assert_list(values, "values in tar_map() must be a list or data frame.")
  assert_nonempty(names(values), "names(values) must not be empty.")
  assert_unique(names(values), "names(values) must be unique.")
  assert_chr(names(values), "names(values) must be a character.")
  assert_nzchr(names(values), "names(values) must not have empty strings.")
  assert_names(names(values), "names(values) must be legal symbol names.")
  assert_nonempty(values, "values in tar_map() must not be empty.")
  assert_equal_lengths(values, "values must have equal-length elements.")
}

tar_map_extend_values <- function(targets, values, names) {
  suffix <- tar_map_produce_suffix(values, names)
  for (target in targets) {
    name <- target$settings$name
    assert_not_in(
      name,
      names(values),
      paste("target", name, "cannot be in names(values).")
    )
    values[[name]] <- rlang::syms(make.names(paste(name, suffix, sep = "_")))
  }
  values
}

tar_map_produce_suffix <- function(values, names) {
  data <- values[names] %|||% list(id = seq_along(values[[1]]))
  data <- map(data, ~as.character(unlist(.x)))
  out <- apply(as.data.frame(data), 1, paste, collapse = "_")
  out <- gsub("'", "", out)
  out <- gsub("\"", "", out)
  make.unique(out, sep = "_")
}

tar_map_target <- function(target, values) {
  lapply(
    transpose(values),
    tar_map_iter,
    target = target,
    command = target$command$expr,
    pattern = target$settings$pattern
  )
}

tar_map_iter <- function(values, target, command, pattern) {
  settings <- target$settings
  name <- as.character(values[[settings$name]])
  command <- substitute_expr(command, values)
  pattern <- substitute_expr(pattern, values) %|||% NULL
  tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = target$command$packages,
    library = target$command$library,
    format = settings$format,
    iteration = settings$iteration,
    error = settings$error,
    memory = settings$memory,
    garbage_collection = settings$garbage_collection,
    deployment = settings$deployment,
    priority = settings$priority,
    resources = settings$resources,
    storage = settings$storage,
    retrieval = settings$retrieval,
    cue = target$cue
  )
}
