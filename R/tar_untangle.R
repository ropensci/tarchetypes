tar_untangle <- function(
  path,
  names = NULL,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  all_arguments <- as.list(formals())
  all_arguments$fields <- quote(tidyselect::everything())
  all_arguments$drop_missing <- FALSE
  new_arguments <- as.list(match.call())
  for (name in names(new_arguments)) {
    all_arguments[[name]] <- new_arguments[[name]]
  }
  all_arguments$path <- NULL
  manifest <- do.call(what = targets::tar_manifest, args = all_arguments)
  manifest$resources <- NULL
  tar_untangle_assert_manifest(manifest)
  untangled <- lapply(split(manifest, manifest$name), tar_untangle_manifest)
  front <- c("---\ntitle: \"Untangled pipeline\"\n---")
  lines <- paste(c(front, as.character(untangled)), collapse = "\n\n")
  writeLines(lines, path)
}

tar_untangle_assert_manifest <- function(manifest) {
  tar_assert_in(
    c("name", "command"),
    colnames(manifest),
    msg = paste(
      "name and command must be part of the",
      "fields argument of tar_untangle()."
    )
  )
}

tar_untangle_manifest <- function(manifest) {
  label <- sprintf("```{r %s}", manifest$name)
  options <- character(0L)
  fields <- c(
    "pattern",
    "format",
    "repository",
    "iteration",
    "error",
    "memory",
    "storage",
    "retrieval",
    "deployment",
    "priority"
  )
  for (field in fields) {
    value <- manifest[[field]][[1L]]
    if (anyNA(value) || is.null(value)) {
      next
    }
    if (identical(value, targets::tar_option_get(field))) {
      next
    }
    new_option <- list(value)
    names(new_option) <- field
    options <- c(options, paste("#|", yaml::as.yaml(new_option)))
  }
  options <- c(options, tar_untangle_cue(manifest))
  statement <- paste(manifest$name, "<-", manifest$command)
  paste(c(label, trimws(options), statement, "```"), collapse = "\n")
}

tar_untangle_cue <- function(manifest) {}
