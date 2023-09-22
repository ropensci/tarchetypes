make_psock_cluster <- function(workers) {
  cluster <- parallel::makePSOCKcluster(workers)
  parallel::clusterCall(
    cl = cluster,
    fun = function(packages, library) {
      for (package in packages) {
        library(package, character.only = TRUE, lib.loc = library)
      }
    },
    packages = targets::tar_definition()$command$packages,
    library = targets::tar_definition()$command$library
  )
  cluster
}
