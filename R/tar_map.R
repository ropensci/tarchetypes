#' @title Static branching.
#' @export
#' @description Define collections of new targets.
#' @details `tar_map()` creates collections of new
#'   targets by iterating over a list of arguments
#'   and substituting symbols into commands and pattern statements.
#' @return A list of new target objects.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `targets::tar_pipeline()`.
#' @param values Named list or data frame with values to iterate over.
#'   The names are the names of symbols in the commands and pattern statements,
#'   and the elements are values that get substituted in place of those
#'   symbols. If you want the new values to also be symbols,
#'   use `rlang::syms()` on each respective vector or column.
#' @param names Subset of `names(values)`
#'   used to generate the suffixes in the names of the new targets.
#'   You can supply symbols, a character vector,
#'   or tidyselect helpers like [starts_with()].
#' @examples
#' \dontrun{
#' targets::tar_script({
#'   targets::tar_pipeline(
#'     tarchetypes::tar_map(
#'       targets::tar_target(x, a + b),
#'       targets::tar_target(y, x + a, pattern = map(x)),
#'       values = list(a = 1:2, b = 3:4)
#'     )
#'   )
#' })
#' targets::tar_manifest()
#' }
tar_map <- function(..., values, names = tidyselect::everything()) {
  targets <- unlist(list(...), recursive = TRUE)
  tar_map_assert_values(values)
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, base::names(values))
  values <- tar_map_extend_values(targets, values, names)
  lapply(targets, tar_map_target, values = values)
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
    values[[name]] <- make.names(paste(name, suffix, sep = "_"))
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
  browser()
}
