#' @title Target that downloads URLs.
#' @export
#' @family targets with custom invalidation rules
#' @description Create a target that downloads file from one or more URLs
#'   and automatically reruns when the remote data changes
#'   (according to the ETags or last-modified time stamps).
#' @details `tar_download()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target uses `format = "url"`
#'   (see `targets::tar_target()`) to track files at one or more URLs,
#'   and automatically invalidate the target if the ETags
#'   or last-modified time stamps change. The downstream target
#'   depends on the upstream one, downloads the files,
#'   and tracks them using `format = "file"`.
#' @return A list of two target objects, one upstream and one downstream.
#'   The upstream one watches a URL for changes, and the downstream one
#'   downloads it.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams tar_download_run
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_download(
#'       x,
#'       urls = c("https://httpbin.org/etag/test", "https://r-project.org"),
#'       paths = c("downloaded_file_1", "downloaded_file_2")
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_download <- function(
  name,
  urls,
  paths,
  method = NULL,
  quiet = TRUE,
  mode = "w",
  cacheOK = TRUE,
  extra = NULL,
  headers = NULL,
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
  name <- targets::tar_deparse_language(substitute(name))
  name_url <- paste0(name, "_url")
  targets::tar_assert_chr(urls, "urls must be a character vector.")
  targets::tar_assert_chr(paths, "paths must be a character vector.")
  targets::tar_assert_nonempty(urls, "urls must be nonempty")
  targets::tar_assert_nonempty(paths, "paths must be nonempty.")
  targets::tar_assert_nzchar(urls, "urls must all be nonempty.")
  targets::tar_assert_nzchar(paths, "paths must all be nonempty.")
  if (length(urls) != length(paths)) {
    targets::tar_throw_validate(
      "'urls' has length ",
      length(urls),
      " but 'paths' has length ",
      length(paths),
      "."
    )
  }
  command_url <- substitute(as.character(x), env = list(x = urls))
  command <- substitute(
    tarchetypes::tar_download_run(
      urls = x,
      paths = paths,
      method = method,
      quiet = quiet,
      mode = mode,
      cacheOK = cacheOK,
      extra = extra,
      headers = headers
    ),
    env = list(
      x = as.symbol(name_url),
      paths = paths,
      method = method,
      quiet = quiet,
      mode = mode,
      cacheOK = cacheOK,
      extra = extra,
      headers = headers
    )
  )
  target_url <- targets::tar_target_raw(
    name = name_url,
    command = command_url,
    format = "url",
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
  target_download <- targets::tar_target_raw(
    name = name,
    command = command,
    format = "file",
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
  list(target_url, target_download)
}

#' @title Download multiple URLs and return the local paths.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A character vector of file paths where the URLs
#'   were downloaded.
#' @inheritParams utils::download.file
#' @param urls Character vector of URLs to track and download.
#'   Must be known and declared before the pipeline runs.
#' @param paths Character vector of local file paths to
#'   download each of the URLs.
#'   Must be known and declared before the pipeline runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#'   tarchetypes::tar_download_run(
#'     urls = "https://httpbin.org/etag/test",
#'     paths = tempfile(),
#'     method = NULL,
#'     quiet = TRUE,
#'     mode = "w",
#'     cacheOK = NULL,
#'     extra = NULL,
#'     headers = NULL
#'   )
#' }
tar_download_run <- function(
  urls,
  paths,
  method,
  quiet,
  mode,
  cacheOK,
  extra,
  headers
) {
  for (index in seq_along(urls)) {
    utils::download.file(
      url = urls[index],
      destfile = paths[index],
      method = method,
      quiet = quiet,
      mode = mode,
      extra = extra,
      headers = headers
    )
  }
  paths
}
