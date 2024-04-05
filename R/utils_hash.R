hash_object <- function(object) {
  secretbase::siphash13(x = object)
}

hash_rows <- function(data) {
  make.unique(map_rows(data, hash_object), sep = "_")
}
