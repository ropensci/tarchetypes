#' @title Batched dynamic-within-static branching for data frames.
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
#' @param command1 R code to create named arguments to `command2`.
#'   Must return a data frame with one row per call to `command2`.
#' @param command2 R code to map over the data frame of arguments
#'   produced by `command1`. Must return a data frame.
#' @param columns1 A tidyselect expression to select which columns of `values`
#'   to append to the output of all targets.
#' @param columns2 A tidyselect expression to select which columns of `command1`
#'   output to append to `command2` output.
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
tar_map2 <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  columns1 = tidyselect::everything(),
  columns2 = tidyselect::everything(),
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
  tar_map2_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    columns1 = substitute(columns1),
    columns2 = substitute(columns2),
    combine = combine,
    group = group,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    format = format,
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
