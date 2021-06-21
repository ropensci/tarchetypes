assert_batches <- function(batches) {
  names(batches) <- names(batches) %||% as.character(seq_along(batches))
  for (name in names(batches)) {
    assert_batch(batches[[name]], name)
  }
  reps <- unique(map_int(batches, batch_count_reps))
  msg <- paste(
    "batched tar_rep() targets for tar_rep_map()",
    "must have equal numbers of reps per batch"
  )
  targets::tar_assert_scalar(reps, msg)
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
  length(unique(batch$tar_rep))
}

assert_batch <- function(batch, name) {
  UseMethod("assert_batch")
}

#' @export
assert_batch.default <- function(batch, name) {
  targets::tar_throw_validate(
    "invalid tar_rep_map() dependency ", name,
    ". Upstream tar_rep() targets must be lists ",
    "with iteration = \"list\" or data frames."
  )
}

#' @export
assert_batch.list <- function(batch, name) {
  lapply(
    batch,
    targets::tar_assert_list,
    msg = paste(
      "Invalid batched list target", name, "for tar_rep_map().",
      "Batched list target deps",
      "need iteration = \"list\" in tar_rep()."
    )
  )
  lapply(batch, assert_reps, name = name)
}

#' @export
assert_batch.data.frame <- function(batch, name) {
  assert_reps(batch, name)
}

assert_reps <- function(rep, name) {
  elts <- names(rep)
  for (elt in c("tar_batch", "tar_rep")) {
    targets::tar_assert_dbl(
      rep[[elt]],
      paste(
        "in batched target ", name, " supplied to tar_rep_map(), ", elt,
        ", must be an integer element of upstream batched targets"
      )
    )
  }
}
