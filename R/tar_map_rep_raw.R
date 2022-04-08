#' @title Dynamic batched replication within static branches
#'   for data frames (raw version).
#' @export
#' @family branching
#' @description Define targets for batched replication
#'   within static branches for data frames (raw version).
#' @description This function is like [tar_map_rep()]
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
#'   Columns already in the target output are not appended.
#' @param combine Logical of length 1, whether to statically combine
#'   all the results into a single target downstream.
#' @param format Character of length 1, storage format of the output.
#'   An efficient data frame format like `"feather"` is recommended,
#'   but the default is `"rds"` to avoid incurring extra package
#'   dependencies. See the help file of `targets::tar_target()`
#'   for details on storage formats.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams tar_map
#' @inheritParams tar_rep
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Just a sketch of a Bayesian sensitivity analysis of hyperparameters:
#'   assess_hyperparameters <- function(sigma1, sigma2) {
#'     # data <- simulate_random_data() # user-defined function
#'     # run_model(data, sigma1, sigma2) # user-defined function
#'     # Mock output from the model:
#'     posterior_samples <- stats::rnorm(1000, 0, sigma1 + sigma2)
#'     tibble::tibble(
#'       posterior_median = median(posterior_samples),
#'       posterior_quantile_0.025 = quantile(posterior_samples, 0.025),
#'       posterior_quantile_0.975 = quantile(posterior_samples, 0.975)
#'     )
#'   }
#'   hyperparameters <- tibble::tibble(
#'     scenario = c("tight", "medium", "diffuse"),
#'     sigma1 = c(10, 50, 50),
#'     sigma2 = c(10, 5, 10)
#'   )
#'   tarchetypes::tar_map_rep_raw(
#'     "sensitivity_analysis",
#'     command = quote(assess_hyperparameters(sigma1, sigma2)),
#'     values = hyperparameters,
#'     names = quote(tidyselect::any_of("scenario")),
#'     batches = 2,
#'     reps = 3
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(sensitivity_analysis)
#' })
#' }
tar_map_rep_raw <- function(
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
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
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
  if (!is.null(names)) {
    targets::tar_assert_lang(names)
  }
  if (!is.null(columns)) {
    targets::tar_assert_lang(columns)
  }
  targets::tar_assert_scalar(batches)
  targets::tar_assert_dbl(batches)
  targets::tar_assert_scalar(reps)
  targets::tar_assert_dbl(reps)
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  envir <- targets::tar_option_get("envir")
  command <- tar_raw_command(name, command)
  command <- targets::tar_tidy_eval(as.expression(command), envir, tidy_eval)
  if (!is.null(values)) {
    columns <- targets::tar_tidyselect_eval(columns, colnames(values))
    command <- tar_command_append_static_values(command, columns)
  }
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
    command = tar_rep_command_target(command, name_batch, reps, "vector"),
    pattern = tar_rep_pattern(name_batch),
    packages = packages,
    library = library,
    format = format,
    repository = repository,
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
    is.null(values) || !combine,
    NULL,
    tar_combine_raw(
      name = name,
      target_static,
      command = tar_map_combine_command,
      use_names = TRUE,
      packages = character(0),
      format = format,
      repository = repository,
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

tar_map_combine_command <- expression({
  out <- dplyr::bind_rows(!!!.x, .id = "tar_group")
  out <- dplyr::mutate(out, tar_group = as.integer(as.factor(tar_group)))
  dplyr::select(out, -tidyselect::any_of("tar_group"), tar_group)
})

tar_command_append_static_values <- function(command, columns) {
  column_syms <- rlang::syms(columns)
  names(column_syms) <- columns
  values <- rlang::call2("list", !!!column_syms)
  rlang::call2(
    .fn = "tar_append_static_values",
    object = command,
    values = values,
    .ns = "tarchetypes"
  )
}

#' @title Append statically mapped values to target output.
#' @export
#' @keywords internal
#' @description For internal use only. Users should not invoke
#'   this function directly.
#' @param object Return value of a target. Must be a data frame.
#' @param values Tibble with the set of static values that the current target
#'   uses.
tar_append_static_values <- function(object, values) {
  if (!length(values)) {
    return(object)
  }
  targets::tar_assert_df(object)
  args <- list(.data = object)
  for (name in setdiff(names(values), names(object))) {
    args[[name]] <- if_any(
      length(values[[name]]) == 1L,
      values[[name]],
      list(values[[name]])
    )
  }
  do.call(dplyr::mutate, args = args)
}
