#' @title Static aggregation.
#' @export
#' @family branching
#' @description Aggregate the results of upstream targets
#'   into a new target.
#' @return A new target object to combine the return values
#'   from the upstream targets.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param name Symbol, name of the new target.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `list()`.
#' @param command R command to aggregate the targets. Must contain
#'   `!!!.x` where the arguments are to be inserted,
#'   where `!!!` is the unquote splice operator from `rlang`.
#' @param use_names Logical, whether to insert the names of the targets
#'   into the command when splicing.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   target1 <- targets::tar_target(x, head(mtcars))
#'   target2 <- targets::tar_target(y, tail(mtcars))
#'   target3 <- tarchetypes::tar_combine(
#'     new_target_name,
#'     target1,
#'     target2,
#'     command = bind_rows(!!!.x)
#'   )
#'   list(target1, target2, target3)
#' })
#' targets::tar_manifest()
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
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
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
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
