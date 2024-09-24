#' @rdname tar_knit
#' @export
tar_knit_raw <- function(
  name,
  path,
  output_file = NULL,
  working_directory = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description"),
  quiet = TRUE,
  knit_arguments = quote(list())
) {
  targets::tar_assert_package("knitr")
  targets::tar_assert_file(path)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_chr(output_file %|||% "x")
  targets::tar_assert_scalar(output_file %|||% "x")
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  targets::tar_assert_lang(knit_arguments)
  targets::tar_assert_not_expr(knit_arguments)
  targets::tar_target_raw(
    name = name,
    command = tar_knit_command(
      path,
      output_file,
      working_directory,
      knit_arguments,
      quiet
    ),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
