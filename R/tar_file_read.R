#' @title Track a file and read the contents.
#' @export
#' @family Simple files
#' @description Create a pair of targets: one to
#'   track a file with `format = "file"`, and another
#'   to read the file.
#' @return A list of two new target objects to track a file
#'   and read the contents.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param command R code that runs in the `format = "file"` target
#'   and returns the file to be tracked.
#' @param read R code to read the file. Must include `!!.x`
#'   where the file path goes: for example,
#'   `read = readr::read_csv(file = !!.x, col_types = readr::cols())`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tar_file_read(data, get_path(), read_csv(file = !!.x, col_types = cols()))
#' })
#' targets::tar_manifest()
#' })
#' }
tar_file_read <- function(
  name,
  command,
  read,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
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
  name_read <- deparse(rlang::enexpr(name))
  name_file <- paste0(name_read, "_file")
  sym_file <- rlang::sym(name_file)
  command <- targets::tar_tidy_eval(
    expr = as.expression(substitute(command)),
    envir = targets::tar_option_get("envir"),
    tidy_eval = tidy_eval
  )
  read <- targets::tar_tidy_eval(
    expr = as.expression(substitute(read)),
    envir = list(.x = sym_file),
    tidy_eval = TRUE
  )
  list(
    targets::tar_target_raw(
      name = name_file,
      command = command,
      packages = packages,
      library = library,
      format = "file",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = deployment,
      priority = priority,
      storage = storage,
      retrieval = retrieval,
      cue = cue
    ),
    targets::tar_target_raw(
      name = name_read,
      command = read,
      packages = packages,
      library = library,
      format = format,
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
  )
}
