#' @title Static aggregation
#' @export
#' @family static branching
#' @description Aggregate the results of upstream targets
#'   into a new target.
#'
#'   [tar_combine()] expects unevaluated expressions for the `name`,
#'   and `command` arguments, whereas [tar_combine_raw()]
#'   uses a character string for `name` and an evaluated expression object
#'   for `command`. See the examples for details.
#' @return A new target object to combine the return values
#'   from the upstream targets.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param name Name of the new target.
#'   [tar_combine()] expects unevaluated expressions for the `name`,
#'   and `command` arguments, whereas [tar_combine_raw()]
#'   uses a character string for `name` and an evaluated expression object
#'   for `command`. See the examples for details.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `list()`.
#' @param command R command to aggregate the targets. Must contain
#'   `!!!.x` where the arguments are to be inserted,
#'   where `!!!` is the unquote splice operator from `rlang`.
#'
#'   [tar_combine()] expects unevaluated expressions for the `name`,
#'   and `command` arguments, whereas [tar_combine_raw()]
#'   uses a character string for `name` and an evaluated expression object
#'   for `command`. See the examples for details.
#' @param use_names Logical, whether to insert the names of the targets
#'   into the command when splicing.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   target1 <- tar_target(x, head(mtcars))
#'   target2 <- tar_target(y, tail(mtcars))
#'   target3 <- tar_combine(
#'     name = new_target_name,
#'     target1,
#'     target2,
#'     command = dplyr::bind_rows(!!!.x)
#'   )
#'   target4 <- tar_combine(
#'     name = new_target_name2,
#'     target1,
#'     target2,
#'     command = dplyr::bind_rows(!!!.x)
#'   )
#'   list(target1, target2, target3, target4)
#' })
#' targets::tar_make()
#' })
#' }
tar_combine <- function(
  name,
  ...,
  command = vctrs::vec_c(!!!.x),
  use_names = TRUE,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  tar_combine_impl(
    name = targets::tar_deparse_language(substitute(name)),
    targets = unlist(list(...), recursive = TRUE),
    command = as.expression(substitute(command)),
    use_names = use_names,
    pattern = as.expression(substitute(pattern)),
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
