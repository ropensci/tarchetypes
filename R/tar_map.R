#' @title Static branching.
#' @export
#' @family static branching
#' @description Define multiple new targets based on existing target objects.
#' @details `tar_map()` creates collections of new
#'   targets by iterating over a list of arguments
#'   and substituting symbols into commands and pattern statements.
#' @section Target objects:
#'   Most `tarchetypes` functions are target factories,
#'   which means they return target objects
#'   or lists of target objects.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please read the walkthrough at
#'   <https://books.ropensci.org/targets/walkthrough.html>
#'   to understand the role of target objects in analysis pipelines.
#'
#'   For developers,
#'   <https://wlandau.github.io/targetopia/contributing.html#target-factories>
#'   explains target factories (functions like this one which generate targets)
#'   and the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   details the structure and composition of target objects.
#' @return A list of new target objects. If `unlist` is `FALSE`,
#'   the list is nested and sub-lists are named and grouped by the original
#'   input targets. If `unlist = TRUE`, the return value is a flat list of
#'   targets named by the new target names.
#'   See the "Target objects" section for background.
#' @param values Named list or data frame with values to iterate over.
#'   The names are the names of symbols in the commands and pattern
#'   statements, and the elements are values that get substituted
#'   in place of those symbols. [tar_map()] uses these elements
#'   to create new R code, so they should be basic types, symbols,
#'   or R expressions. For objects even a little bit complicated,
#'   especially objects with attributes, it is not obvious how to
#'   convert the object into code that generates it.
#'   For complicated objects, consider using `quote()` when
#'   you define `values`, as shown at
#'   <https://github.com/ropensci/tarchetypes/discussions/105>.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `list()`.
#' @param names Subset of `names(values)`
#'   used to generate the suffixes in the names of the new targets.
#'   The value of `names` should be a `tidyselect` expression
#'   such as a call to [any_of()] or [starts_with()].
#' @param descriptions Names of a column in `values`
#'   to append to the custom description of each generated target.
#'   The value of `descriptions` should be a `tidyselect` expression
#'   such as a call to [any_of()] or [starts_with()].
#' @param unlist Logical, whether to flatten the returned list of targets.
#'   If `unlist = FALSE`, the list is nested and sub-lists
#'   are named and grouped by the original input targets.
#'   If `unlist = TRUE`, the return value is a flat list of targets
#'   named by the new target names.
#' @param delimiter Character of length 1, string to insert between
#'   other strings when creating names of targets.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_map(
#'       list(a = c(12, 34), b = c(45, 78)),
#'       targets::tar_target(x, a + b),
#'       targets::tar_target(y, x + a, pattern = map(x))
#'     )
#'   )
#' })
#' targets::tar_manifest()
#' })
#' }
tar_map <- function(
  values,
  ...,
  names = tidyselect::everything(),
  descriptions = tidyselect::everything(),
  unlist = FALSE,
  delimiter = "_"
) {
  targets::tar_assert_chr(delimiter)
  targets::tar_assert_scalar(delimiter)
  targets <- unlist(list(...), recursive = TRUE) %|||% list()
  targets::tar_assert_target_list(targets)
  assert_values_list(values)
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, base::names(values))
  descriptions_quosure <- rlang::enquo(descriptions)
  descriptions <- eval_tidyselect(descriptions_quosure, base::names(values))
  values <- tibble::as_tibble(values)
  values <- tar_map_process_values(values)
  values <- tar_map_extend_values(targets, values, names, delimiter)
  out <- lapply(
    X = targets,
    FUN = tar_map_target,
    values = values,
    descriptions = descriptions
  )
  flat <- unlist(out, recursive = TRUE)
  if_any(
    unlist,
    set_names(flat, map_chr(flat, ~.x$settings$name)),
    set_names(out, map_chr(targets, ~.x$settings$name))
  )
}

tar_map_process_values <- function(values) {
  for (name in names(values)) {
    values[[name]] <- map(
      values[[name]],
      ~parse(text = targets::tar_deparse_safe(.x))[[1]]
    )
  }
  values
}

tar_map_extend_values <- function(targets, values, names, delimiter) {
  suffix <- tar_map_produce_suffix(values, names, delimiter)
  for (target in targets) {
    name <- target$settings$name
    targets::tar_assert_not_in(
      name,
      names(values),
      paste("target", name, "cannot be in names(values).")
    )
    values[[name]] <- as_symbols(
      make.names(paste(name, suffix, sep = delimiter))
    )
  }
  values
}

tar_map_produce_suffix <- function(values, names, delimiter) {
  data <- values[names] %||% tar_map_default_suffixes(values)
  data <- map(data, ~as.character(unlist(.x)))
  out <- apply(as.data.frame(data), 1, paste, collapse = delimiter)
  out <- gsub("'", "", out)
  out <- gsub("\"", "", out)
  make.unique(out, sep = delimiter)
}

tar_map_default_suffixes <- function(values) {
  list(id = apply(X = values, MARGIN = 1, FUN = hash_object))
}

tar_map_target <- function(target, values, descriptions) {
  out <- lapply(
    X = transpose(values),
    FUN = tar_map_iter,
    target = target,
    command = target$command$expr,
    pattern = target$settings$pattern,
    descriptions = descriptions
  )
  names(out) <- map_chr(out, ~.x$settings$name)
  out
}

tar_map_iter <- function(values, target, command, pattern, descriptions) {
  settings <- target$settings
  name <- as.character(values[[settings$name]])
  command <- substitute_expr(command, values)
  pattern <- substitute_expr(pattern, values) %||% NULL
  description <- tar_map_extend_description(
    description = settings$description,
    values = values,
    descriptions = descriptions
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = target$command$packages,
    library = target$command$library,
    format = settings$format,
    repository = settings$repository,
    iteration = settings$iteration,
    error = settings$error,
    memory = settings$memory,
    garbage_collection = settings$garbage_collection,
    deployment = settings$deployment,
    priority = settings$priority,
    resources = settings$resources,
    storage = settings$storage,
    retrieval = settings$retrieval,
    cue = targets::tar_cue(
      mode = target$cue$mode,
      command = target$cue$command,
      depend = target$cue$depend,
      format = target$cue$format,
      repository = target$cue$repository,
      iteration = target$cue$iteration,
      file = target$cue$file
    ),
    description = description
  )
}

tar_map_extend_description <- function(description, values, descriptions) {
  description <- as.character(description)
  if (length(descriptions) > 0L) {
    suffix <- paste(as.character(values[descriptions]), collapse = " ")
    description <- paste(description, suffix)
  }
  trimws(description)
}
