step_new <- function() {
  new.env(hash = TRUE, parent = emptyenv())
}

step_set <- function(step, batch, rep, reps) {
  step$batch <- as.integer(batch)
  step$rep <- as.integer(rep)
  step$index <- as.integer(rep + (reps * (batch - 1)))
}

step_reset <- function(step) {
  step$batch <- NULL
  step$rep <- NULL
  step$index <- NULL
}

step_tar_rep <- step_new()
