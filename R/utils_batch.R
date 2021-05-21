assert_batches <- function(batches) {
  lapply(batches, assert_batch)
  reps <- unique(map_int(batches, batch_count_reps))
  msg <- "batches supplied to tar_rep_map() must have equal numbers of reps"
  assert_scalar(reps, msg)
}

batch_count_reps <- function(batch) {
  UseMethod("batch_count_reps")
}

#' @export
batch_count_reps.list <- function(batch) {
  length(batch)
}

#' @export
batch_count_reps.data.frame <- function(batch) {
  nrow(batch)
}

assert_batch <- function(batch) {
  UseMethod("assert_batch")
}

#' @export
assert_batch.default <- function(batch) {
  throw_validate(
    "Invalid tar_rep_map() batch. ",
    "Upstream tar_rep() batches must be lists ",
    "with iteration = \"list\" or data frames."
  )
}

#' @export
assert_batch.list <- function(batch) {
  lapply(batch, assert_reps)
}

#' @export
assert_batch.data.frame <- function(batch) {
  assert_reps(batch)
}

assert_reps <- function(rep) {
  elts <- names(rep)
  for (elt in c("tar_batch", "tar_rep")) {
    assert_dbl(
      rep[[elt]],
      paste(
        "in tar_rep_map(), ",
        elt,
        ", must be an integer element of upstream tar_rep() targets"
      )
    )
  }
}
