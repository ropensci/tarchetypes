assert_batches <- function(batches) {
  for (name in names(batches)) {
    assert_batch(batches[[name]], name)
  }
  reps <- unique(map_int(batches, batch_count_reps))
  msg <- "batched targets for tar_map_reps() must have equal numbers of reps"
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
  length(unique(batch$tar_rep))
}

assert_batch <- function(batch, name) {
  UseMethod("assert_batch")
}

#' @export
assert_batch.default <- function(batch, name) {
  throw_validate(
    "Invalid tar_map_reps() batch from target ", name,
    ". Upstream batched targets must either be data frames ",
    "or lists that use iteration = \"list\"."
  )
}

#' @export
assert_batch.list <- function(batch, name) {
  lapply(
    batch,
    assert_list,
    msg = paste(
      "Invalid batched list target", name, "for tar_map_reps().",
      "Batched list target deps",
      "need iteration = \"list\" in the target definition."
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
    assert_dbl(
      rep[[elt]],
      paste(
        "in batched target ", name, " supplied to tar_map_reps(), ", elt,
        ", must be an integer element of upstream batched targets"
      )
    )
  }
}
