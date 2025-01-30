targets::tar_test("step class works", {
  step <- step_new()
  index <- 0L
  batches <- 5L
  reps <- 7L
  for (batch in seq_len(batches)) {
    for (rep in seq_len(reps)) {
      index <- index + 1L
      step_set(step = step, batch = batch, rep = rep, reps = reps)
      expect_equal(step$batch, batch)
      expect_equal(step$rep, rep)
      expect_equal(step$index, index)
    }
  }
  step_reset(step = step)
  for (field in c("batch", "rep", "index")) {
    expect_null(step[[field]])
  }
})
