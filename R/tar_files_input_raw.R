#' @rdname tar_files_input
#' @export
tar_files_input_raw <- function(
  name,
  files,
  batches = length(files),
  format = c("file", "file_fast", "url", "aws_file"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  targets::tar_assert_chr(name, "name must be a character.")
  targets::tar_assert_scalar(name, "name must have length 1.")
  targets::tar_assert_chr(files, "files must be a character vector.")
  targets::tar_assert_nonempty(files, "files must have length > 0.")
  targets::tar_assert_dbl(batches, "batches must be numeric.")
  targets::tar_assert_scalar(batches, "batches must have length 1.")
  format <- match.arg(format)
  name_files <- paste0(name, "_files")
  files <- tar_files_input_batch_files(files, batches)
  upstream <- targets::tar_target_raw(
    name = name_files,
    command = parse(text = targets::tar_deparse_safe(files, collapse = " ")),
    pattern = NULL,
    packages = character(0),
    format = "rds",
    repository = repository,
    iteration = "list",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    resources = resources,
    storage = "main",
    retrieval = "main",
    cue = cue,
    description = description
  )
  name_files_sym <- as.symbol(name_files)
  downstream <- targets::tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(call_function("map", list(name_files_sym))),
    packages = character(0),
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    resources = resources,
    storage = "main",
    retrieval = "main",
    cue = cue,
    description = description
  )
  out <- list(upstream, downstream)
  names(out) <- c(name_files, name)
  out
}

tar_files_input_batch_files <- function(files, batches) {
  batches <- min(batches, length(files))
  index <- if_any(
    batches > 1L,
    as.integer(cut(seq_along(files), breaks = batches)),
    rep(1L, length(files))
  )
  unname(split(files, f = index))
}
