#' @title Target formats
#' @name tar_formats
#' @family Formats
#' @description Target archetypes for specialized storage formats.
#' @details These functions are shorthand for targets with specialized
#'   storage formats. For example, `tar_qs(name, fun())` is equivalent to
#'   `tar_target(name, fun(), format = "qs")`.
#'   For details on specialized storage formats, open the help file of the
#'   `targets::tar_target()` function and read about the `format` argument.
#' @return A `tar_target()` object with the eponymous storage format.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script(
#'   list(
#'     tarchetypes::tar_rds(x, 1)
#'   )
#' )
#' targets::tar_make()
#' })
#' }
NULL

tar_format <- function(format) {
  out <- tar_format_api
  body(out) <- substitute({
    name <- targets::tar_deparse_language(substitute(name))
    envir <- tar_option_get("envir")
    command <- targets::tar_tidy_eval(
      as.expression(substitute(command)),
      envir,
      tidy_eval
    )
    pattern <- targets::tar_tidy_eval(
      as.expression(substitute(pattern)),
      envir,
      tidy_eval
    )
    targets::tar_target_raw(
      name = name,
      command = command,
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
  }, env = list(format = format))
  out
}

tar_format_api <- function(
  name,
  command,
  pattern = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
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
}

#' @export
#' @rdname tar_formats
tar_url <- tar_format("url")

#' @export
#' @rdname tar_formats
tar_file <- tar_format("file")

#' @export
#' @rdname tar_formats
tar_rds <- tar_format("rds")

#' @export
#' @rdname tar_formats
tar_qs <- tar_format("qs")

#' @export
#' @rdname tar_formats
tar_keras <- tar_format("keras")

#' @export
#' @rdname tar_formats
tar_torch <- tar_format("torch")

#' @export
#' @rdname tar_formats
tar_format_feather <- tar_format("feather")

#' @export
#' @rdname tar_formats
tar_parquet <- tar_format("parquet")

#' @export
#' @rdname tar_formats
tar_fst <- tar_format("fst")

#' @export
#' @rdname tar_formats
tar_fst_dt <- tar_format("fst_dt")

#' @export
#' @rdname tar_formats
tar_fst_tbl <- tar_format("fst_tbl")

#' @export
#' @rdname tar_formats
tar_aws_file <- tar_format("aws_file")

#' @export
#' @rdname tar_formats
tar_aws_rds <- tar_format("aws_rds")

#' @export
#' @rdname tar_formats
tar_aws_qs <- tar_format("aws_qs")

#' @export
#' @rdname tar_formats
tar_aws_keras <- tar_format("aws_keras")

#' @export
#' @rdname tar_formats
tar_aws_torch <- tar_format("aws_torch")

#' @export
#' @rdname tar_formats
tar_format_aws_feather <- tar_format("aws_feather")

#' @export
#' @rdname tar_formats
tar_aws_parquet <- tar_format("aws_parquet")

#' @export
#' @rdname tar_formats
tar_aws_fst <- tar_format("aws_fst")

#' @export
#' @rdname tar_formats
tar_aws_fst_dt <- tar_format("aws_fst_dt")

#' @export
#' @rdname tar_formats
tar_aws_fst_tbl <- tar_format("aws_fst_tbl")
