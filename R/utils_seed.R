# TODO: use targets::tar_seed_create() when CRAN targets has it.
tar_seed_create <- function(name, global_seed = NULL) {
  if (is.null(global_seed)) {
    global_seed <- targets::tar_option_get("seed")
  }
  if (is.null(global_seed) || anyNA(global_seed)) {
    return(NA_integer_)
  }
  name <- as.character(name)
  hash <- digest::digest(
    object = name,
    algo = "sha512",
    serialize = FALSE,
    file = FALSE,
    seed = 0L
  )
  digest::digest2int(x = hash, seed = global_seed)
}

# TODO: same
tar_seed_set <- function(seed) {
  if (!is.null(seed) && !anyNA(seed)) {
    set.seed(
      seed = seed,
      kind = "default",
      normal.kind = "default",
      sample.kind = "default"
    )
  }
  invisible()
}
