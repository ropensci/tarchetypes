#' @title Download a file from a remote source, checking for changes
#'   first
#' @description Create a target that downnloads a file if it has changed since
#'   the last attempt, based responds to a change
#'   in an arbitrary value. If the value changes, the target reruns.
#' @details `tar_download()` creates a pair of targets, one upstream
#'   and one downstream, usingn `tar_change()`. always runs and returns
#'   header information (eTag and modified time, if available), to check
#'   if the remote value has changed. This header gets referenced in the
#'   downstream target, which causes the downstream target to download the
#'   if the header information changes. The downstream target has "file" format
#'   and stores the location of te downloaded file The behavior is cancelled if
#'   `cue` is `tar_cue(depend = FALSE)` or `tar_cue(mode = "never")`.
#' @export
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one checks url headers, and the downstream one
#'   downloads the file to it. See the examples for details.
#' @param url The URL to fetch the file from
#' @param destfile Whether to invoke tidy evaluation
#' @param destdir the directory to download the file to 
#' @param stop_on_no_internet Whether to fail in the absence of an internet connection.
#' Default is `FALSE` and only produces a warning
#' @param handle a handle created by `curl::new_handle()` to use when fetching the file
#'   or checking headers. Useful for authentication.
#' @param ... other parameters passed to `tar_change()`
#' @examples
#' \dontrun{
#' 
# targets::tar_dir({
# targets::tar_script({
#   library(tarchetypes)
#   tar_pipeline(
#     tar_download(gitfile, url = "https://github.com/wlandau/tarchetypes/archive/master.zip")
#   )
# })
# targets::tar_make(callr_function = NULL)
# })
#' }
tar_download <- function(
  name,
  url,
  destfile = basename(url),
  destdir = ".",
  handle = NULL,
  stop_on_no_internet = FALSE,
  ...
) {
  if(!requireNamespace("curl", quietly = TRUE)) {
    stop("tar_download() requires the the package 'curl' to be installed")
  }
  handle <- handle %||% curl::new_handle()
  tar_change(
    name,
    command = substitute({
      tar_download_file(url, destfile, destdir, stop_on_no_internet, handle)
      }, env = list(url = url, destfile = destfile, destdir = destdir, stop_on_no_internet = stop_on_no_internet, handle = handle)),
    change = substitute({
      tar_url_hash(url, stop_on_no_internet, handle)
      },  env = list(url = url, stop_on_no_internet = stop_on_no_internet, handle = handle)),
    format = "file"
    ...
  )
}

#' @rdname tar_download
#' @export
tar_download_file <- function(url, destfile, destdir, stop_on_no_internet = FALSE, handle = curl::new_handle())
  {
  if (!curl::has_internet()) {
    if (stop_on_no_internet)
      stop("No internet. Cannot downoad url: ", url)
    else
      warning("No internet. Cannot download url: ", url)
      tar_cancel()
  }
  outpath <- curl::curl_download(url, destfile = fs::path(destdir, destfile), handle = curl::new_handle())
  outpath
}

#' @rdname tar_download
#' @export
tar_url_hash <- function(url, stop_on_no_internet = FALSE, handle = NULL) {
  if (!curl::has_internet()) {
    if (stop_on_no_internet)
      stop("No internet. Cannot check url: ", url)
    else
      warning("No internet. Cannot check url: ", url)
      tar_cancel()
  }
  handle <- curl::handle_setopt(handle, nobody = TRUE)
  req <- curl::curl_fetch_memory(url, handle = handle)
  stopifnot(length(req$content) < 1L)
  headers <- curl::parse_headers_list(req$headers)
  assert_status_code(req, url)
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  return(paste(etag, mtime))
}

assert_status_code <- function(req, url) {
  if (req$status_code != 200L) {
    stop("could not access url: ", url)
  }
}
