hash_rows <- function(data) {
  out <- map_rows(
    data,
    ~digest::digest(.x, algo = "xxhash32")
  )
  make.unique(out, sep = "_")
}
