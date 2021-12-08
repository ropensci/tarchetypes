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
#' @param name Character of length 1, base name of the targets.
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
#'   In case of conflicts, `column1` takes precedence.
#' @param group Function on the data produced by `command1` to create the
#'   `tar_group` column that determines the batching structure for the
#'   `command2` targets.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map_rep_raw
#' @inheritParams tar_rep2
#' @inheritParams tar_map
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2_raw(
#'     "x",
#'     command1 = quote(
#'       tibble::tibble(
#'         arg1 = arg1,
#'         arg2 = sample.int(12)
#'       )
#'     ),
#'     command2 = quote(
#'       tibble::tibble(
#'         result = paste(arg1, arg2),
#'         length_input = length(arg1)
#'       )
#'     ),
#'     values = tibble::tibble(arg1 = letters[seq_len(12)]),
#'     group = quote(rep(LETTERS[seq_len(2)], each = nrow(.x) / 2))
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
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
  group = quote(rep(1L, nrow(.x))),
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
  if (!is.null(names)) {
    targets::tar_assert_lang(names)
  }
  if (!is.null(columns1)) {
    targets::tar_assert_lang(columns1)
  }
  if (!is.null(columns2)) {
    targets::tar_assert_lang(columns2)
  }
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  targets::tar_assert_lang(group)
  envir <- targets::tar_option_get("envir")
  command1 <- tar_raw_command(name, command1)
  command2 <- tar_raw_command(name, command2)
  command1 <- targets::tar_tidy_eval(as.expression(command1), envir, tidy_eval)
  command2 <- targets::tar_tidy_eval(as.expression(command2), envir, tidy_eval)
  if (!is.null(values)) {
    columns1 <- targets::tar_tidyselect_eval(columns1, colnames(values))
    command1 <- tar_command_append_static_values(command1, columns1)
    command2 <- tar_command_append_static_values(command2, columns1)
  }
  name_upstream <- paste0(name, "_1")
  name_downstream <- paste0(name, "_2")
  sym_upstream <- as.symbol(name_upstream)
  sym_downstream <- as.symbol(name_downstream)
  target_upstream <- targets::tar_target_raw(
    name = name_upstream,
    command = tar_map2_command_upstream(command1, group),
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
  target_downstream <- targets::tar_target_raw(
    name = name_downstream,
    command = tar_map2_command_downstream(command2, sym_upstream, columns2),
    pattern = rlang::call2("map", sym_upstream),
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
    list(target_upstream, target_downstream),
    do.call(
      tar_map,
      args = list(
        target_upstream,
        target_downstream,
        values = values,
        names = names,
        unlist = FALSE
      )
    )
  )
  target_combine <- if_any(
    is.null(values) || !combine,
    NULL,
    tar_combine_raw(
      name = name,
      target_static[[name_downstream]],
      command = tar_map_combine_command,
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
  unlist(list(target_static, target_combine), recursive = TRUE)
}

tar_map2_command_upstream <- function(command, group) {
  rlang::call2(
    "tar_map2_group",
    data = command,
    group = group,
    .ns = "tarchetypes"
  )
}

tar_map2_command_downstream <- function(command, sym_upstream, columns2) {
  rlang::call2(
    "tar_map2_run",
    data = command,
    values = sym_upstream,
    columns = columns2,
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
tar_map2_group <- function(data, group) {
  if (!is.null(group)) {
    expr <- targets::tar_tidy_eval(
      group,
      envir = list(.x = data),
      tidy_eval = TRUE
    )
    out <- eval(expr, envir = targets::tar_option_get("envir"))
    data[["tar_group"]] <- as.integer(as.factor(out))
  }
  data
}

#' @title Run a dynamic batch of a `tar_map2()` target.
#' @export
#' @keywords internal
#' @details For internal use only. Users should not invoke
#'   this function directly.
#' @return A data frame with a `tar_group` column attached (if `group`
#'   is not `NULL`).
#' @param command Command to run.
#' @param values Data frame of named arguments to map over.
#' @param columns tidyselect expression to select columns of `values`
#'   to append to the result.
tar_map2_run <- function(command, values, columns) {
  command <- substitute(command)
  columns <- substitute(columns)
  out <- lapply(
    split(values, f = seq_len(nrow(values))),
    tar_map2_run_rep,
    command = command
  )
  do.call(vctrs::vec_rbind, out)
}

tar_map2_run_rep <- function(command, values) {
  envir <- targets::tar_envir()
  names <- names(values)
  lapply(
    X = seq_len(ncol(values)),
    FUN = function(index) {
      assign(
        x = names[index],
        value = values[[index]],
        envir = envir
      )
    }
  )
  out <- eval(command, envir = targets::tar_envir())
  columns <- targets::tar_tidyselect_eval(columns, colnames(values))
  columns <- setdiff(columns, colnames(out))
  tar_append_static_values(out, values[, columns])
}