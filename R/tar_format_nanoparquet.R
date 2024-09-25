#' @title Nanoparquet format
#' @export
#' @keywords storage formats
#' @description Nanoparquet storage format for data frames.
#'   Uses [nanoparquet::read_parquet()] and [nanoparquet::write_parquet()]
#'   to read and write data frames returned by targets in a pipeline.
#'   Note: attributes such as `dplyr` row groupings and `posterior`
#'   draws info are dropped during the writing process.
#' @return A [targets::tar_format()] storage format specification string
#'   that can be directly supplied to the `format` argument of
#'   [targets::tar_target()] or [targets::tar_option_set()].
#' @param compression Character string, compression type for saving the
#'   data. See the `compression` argument of [nanoparquet::write_parquet()]
#'   for details.
#' @param class Character vector with the data frame subclasses to assign.
#'   See the `class` argument of [nanoparquet::parquet_options()] for details.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(targets)
#'   libary(tarchetypes)
#'   list(
#'     tar_target(
#'       name = data,
#'       command = data.frame(x = 1),
#'       format = tar_format_nanoparquet()
#'     )
#'   )
#' })
#' tar_make()
#' tar_read(data)
#' })
#' }
tar_format_nanoparquet <- function(compression = "snappy", class = "tbl") {
  rlang::check_installed("nanoparquet")
  read <- function(path) {}
  body(read) <- substitute(
    tarchetypes::tar_nanoparquet_read(path, class),
    env = list(class = class)
  )
  write <- function(object, path) {}
  body(write) <- substitute(
    tarchetypes::tar_nanoparquet_write(object, path, compression),
    env = list(compression = compression)
  )
  convert <- function(object) {}
  body(convert) <- substitute(
    tarchetypes::tar_nanoparquet_convert(object, class),
    env = list(class = class)
  )
  targets::tar_format(read = read, write = write, convert = convert)
}

#' @title Nanoparquet read method
#' @export
#' @keywords internal
#' @description Internal function.
#' @param path Path to the data.
#' @param class S3 classes to assign to the returned object.
tar_nanoparquet_read <- function(path, class) {
  nanoparquet::read_parquet(
    file = path,
    options = nanoparquet::parquet_options(
      class = class,
      use_arrow_metadata = TRUE
    )
  )
}

#' @title Nanoparquet write method
#' @export
#' @keywords internal
#' @description Internal function.
#' @param object R object to save.
#' @param path Path to the data.
#' @param compression Compression type.
tar_nanoparquet_write <- function(object, path, compression) {
  nanoparquet::write_parquet(
    x = object,
    file = path,
    compression = compression,
    options = nanoparquet::parquet_options(write_arrow_metadata = TRUE)
  )
}

#' @title Nanoparquet convert method
#' @export
#' @keywords internal
#' @description Internal function.
#' @param object R object to convert.
#' @param class S3 classes to assign to the returned object.
tar_nanoparquet_convert <- function(object, class) {
  class(object) <- c(class, "data.frame")
  object
}
