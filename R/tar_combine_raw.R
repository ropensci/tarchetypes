#' @title Static aggregation (raw version).
#' @export
#' @description Like [tar_combine()] except the `name`, `command`,
#'   and `pattern` arguments use standard evaluation.
#' @return A new target object to combine the return values
#'   from the upstream targets.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams targets::tar_target_raw
#' @param name Character, name of the new target.
#' @param ... One or more target objects or list of target objects.
#'   Lists can be arbitrarily nested, as in `list()`.
#' @param command Expression object,
#'   R command to aggregate the targets. Must contain
#'   `!!!.x` where the arguments are to be inserted,
#'   where `!!!` is the unquote splice operator from `rlang`.
#' @param use_names Logical, whether to insert the names of the targets
#'   into the command when splicing.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({
#' targets::tar_script({
#'   target1 <- targets::tar_target(x, head(mtcars))
#'   target2 <- targets::tar_target(y, tail(mtcars))
#'   target3 <- tarchetypes::tar_combine(new_target_name, target1, target2)
#'   list(target1, target2, target3)
#' })
#' targets::tar_manifest()
#' })
#' }
tar_combine_raw <- function(
  name,
  ...,
  command = expression(vctrs::vec_c(!!!.x)),
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
  targets <- unlist(list(...), recursive = TRUE)
  tar_combine_impl(
    name = name,
    targets = targets,
    command = command,
    use_names = use_names,
    pattern = pattern,
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
