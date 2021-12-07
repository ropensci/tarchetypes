#' @title Batched dynamic-within-static branching for data frames (raw version).
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames.
#' @details Static branching creates one pair of targets
#'   for each row in `values`. In each pair,
#'   there is an upstream non-dynamic target that runs `command1`
#'   and a downstream dynamic target that runs `command2`.
#'   `command1` produces a data frame of arguments to
#'   `command2`, and `command2` dynamically maps over
#'   these arguments in batches.
#' @return A list of new target objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param command1 Language object to create named arguments to `command2`.
#'   Must return a data frame with one row per call to `command2`.
#' @param command2 Language object  to map over the data frame of arguments
#'   produced by `command1`. Must return a data frame.
#' @param columns1 Language object, a tidyselect expression
#'   to select which columns of `values`
#'   to append to the output of all targets.
#' @param columns2 Language object, a tidyselect expression
#'   to select which columns of `command1`
#'   output to append to `command2` output.
#' @param group Function on the data produced by `command1` to create the
#'   `tar_group` column that determines the batching structure for the
#'   `command2` targets.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map2_raw
#' @inheritParams tar_rep2
#' @inheritParams tar_map
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
#'   tarchetypes::tar_map2_count(
#'     sensitivity_analysis,
#'     command = assess_hyperparameters(sigma1, sigma2),
#'     values = hyperparameters,
#'     names = tidyselect::any_of("scenario"),
#'     batches = 2,
#'     reps = 3
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(sensitivity_analysis)
#' })
#' }
tar_map2_raw <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  columns1 = quote(tidyselect::everything()),
  columns2 = quote(tidyselect::everything()),
  combine = TRUE,
  group = function(x) rep(1L, nrow(x)),
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
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
  if (!is.null(columns1)) {
    targets::tar_assert_lang(columns1)
  }
  if (!is.null(columns2)) {
    targets::tar_assert_lang(columns2)
  }
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  targets::tar_assert_function(group)
  envir <- targets::tar_option_get("envir")
  columns1 <- targets::tar_tidyselect_eval(columns1, colnames(values))
  command1 <- tar_raw_command(name, command1)
  command2 <- tar_raw_command(name, command2)
  command1 <- targets::tar_tidy_eval(as.expression(command1), envir, tidy_eval)
  command2 <- targets::tar_tidy_eval(as.expression(command2), envir, tidy_eval)
  command1 <- tar_command_append_static_values(command1, columns1)
  command2 <- tar_command_append_static_values(command2, columns1)
  target_upstream <- targets::tar_target_raw(
    name = name,
    command = tar_map2_command_upstream(command1, columns, group),
    packages = packages,
    library = library,
    format = format,
    iteration = "group",
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_map2_command_upstream <- function(command, columns, group) {
  out <- tar_command_append_static_values(command = command, columns = columns)
  rlang::call2(
    "tar_command_append_group",
    data = out,
    group = group,
    .ns = "tarchetypes"
  )
}

#' @title Append the `tar_group` variable to a `tar_map2()` target.
#' @export
#' @keywords internal
#' @details For internal use only. Users should not invoke
#'   this function directly.
#' @return A data frame with a `tar_group` column attached (if `group`
#'   is not `NULL`).
#' @param data Data frame to be returned from the target.
#' @param group Function on the data to return the `tar_group` column.
#'   If `group` is `NULL`, then no `tar_group` column is attached.
tar_command_append_group <- function(data, group) {
  if (!is.null(group)) {
    data[["tar_group"]] <- group(data)
  }
  data
}
