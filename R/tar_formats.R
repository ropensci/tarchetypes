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

tar_format_alias <- function(format) {
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
}

#' @export
#' @rdname tar_formats
tar_url <- tar_format_alias("url")

#' @export
#' @rdname tar_formats
tar_file <- tar_format_alias("file")

#' @export
#' @rdname tar_formats
tar_file_fast <- tar_format_alias("file_fast")

#' @export
#' @rdname tar_formats
tar_rds <- tar_format_alias("rds")

#' @export
#' @rdname tar_formats
tar_qs <- tar_format_alias("qs")

#' @export
#' @rdname tar_formats
tar_keras <- tar_format_alias("keras")

#' @export
#' @rdname tar_formats
tar_torch <- tar_format_alias("torch")

#' @export
#' @rdname tar_formats
tar_format_feather <- tar_format_alias("feather")

#' @export
#' @rdname tar_formats
tar_parquet <- tar_format_alias("parquet")

#' @export
#' @rdname tar_formats
tar_fst <- tar_format_alias("fst")

#' @export
#' @rdname tar_formats
tar_fst_dt <- tar_format_alias("fst_dt")

#' @export
#' @rdname tar_formats
tar_fst_tbl <- tar_format_alias("fst_tbl")

#' @export
#' @rdname tar_formats
tar_aws_file <- tar_format_alias("aws_file")

#' @export
#' @rdname tar_formats
tar_aws_rds <- tar_format_alias("aws_rds")

#' @export
#' @rdname tar_formats
tar_aws_qs <- tar_format_alias("aws_qs")

#' @export
#' @rdname tar_formats
tar_aws_keras <- tar_format_alias("aws_keras")

#' @export
#' @rdname tar_formats
tar_aws_torch <- tar_format_alias("aws_torch")

#' @export
#' @rdname tar_formats
tar_format_aws_feather <- tar_format_alias("aws_feather")

#' @export
#' @rdname tar_formats
tar_aws_parquet <- tar_format_alias("aws_parquet")

#' @export
#' @rdname tar_formats
tar_aws_fst <- tar_format_alias("aws_fst")

#' @export
#' @rdname tar_formats
tar_aws_fst_dt <- tar_format_alias("aws_fst_dt")

#' @export
#' @rdname tar_formats
tar_aws_fst_tbl <- tar_format_alias("aws_fst_tbl")
