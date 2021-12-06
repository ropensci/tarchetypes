#' @title Batched replication within static branches for data frames (raw version).
#' @export
#' @family branching
#' @description Define targets for batched replication
#'   within static branches for data frames (raw version).
#' @description This funciton is like [tar_rep_row()]
#'   except the `name` argument is a character string
#'   and the `names` and `columns` arguments are
#'   language objects.
#' @return A list of new target objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param command Language object, R code for a single replicate. Must return
#'   a data frame.
#' @param names Language object with a tidyselect expression
#'   to select which columns of `values` to use to construct
#'   statically branched target names. If `NULL`, then
#'   short names are automatically generated.
#' @param columns Language object with a tidyselect expression
#'   to select which columns of `values` to append to the output.
#' @param combine Logical of length 1, whether to statically combine
#'   all the results into a single target downstream.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_row_rep
#' @examples
tar_rep_row_raw <- function(
  name,
  command,
  values = NULL,
  names = NULL,
  columns = quote(tidyselect::everything()),
  batches = 1,
  reps = 1,
  combine = TRUE,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = "feather",
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_df(values %|||% data.frame())
  if (!is.null(values)) {
    targets::tar_assert_ge(nrow(values), 1L)
  }
  targets::tar_assert_lang(names)
  targets::tar_assert_lang(columns)
  targets::tar_assert_scalar(batches)
  targets::tar_assert_dbl(batches)
  targets::tar_assert_scalar(reps)
  targets::tar_assert_dbl(reps)
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  envir <- targets::tar_option_get("envir")
  command <- tar_raw_command(name, command)
  command <- targets::tar_tidy_eval(as.expression(command), envir, tidy_eval)
# TODO: append columns from values to output.
  name_batch <- paste0(name, "_batch")
  target_batch <- targets::tar_target_raw(
    name = name_batch,
    command = substitute(seq_len(batches), env = list(batches = batches)),
    packages = character(0),
    iteration = "vector",
    error = error,
    memory = memory,
    deployment = "main",
    priority = priority,
    storage = "main",
    retrieval = "main",
    cue = cue
  )
  target_dynamic <- targets::tar_target_raw(
    name = name,
    command = tar_rep_command_target(command, name_batch, reps, iteration),
    pattern = tar_rep_pattern(name_batch),
    packages = packages,
    library = library,
    format = format,
    iteration = "vector",
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
  target_static <- if_any(
    is.null(values),
    target_dynamic,
    do.call(
      tar_map,
      args = list(target_dynamic, values = values, names = names)
    )
  )
  target_combine <- if_any(
    is.null(values) && combine,
    NULL,
    tar_combine_raw(
      name = name,
      target_static,
      command = tar_rep_row_combine_command,
      use_names = TRUE,
      packages = character(0),
      format = format,
      iteration = "group",
      error = error,
      memory = memory,
      deployment = "main",
      priority = priority,
      cue = cue
    )
  )
  unlist(list(target_batch, target_static, target_combine), recursive = TRUE)
}

tar_rep_row_combine_command <- expression({
  out <- dplyr::bind_rows(!!!.x, .id = "tar_group")
  dplyr::mutate(out, tar_group = as.integer(as.factor(tar_group)))
})
