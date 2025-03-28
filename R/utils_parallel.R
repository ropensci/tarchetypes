make_psock_cluster <- function(workers, export = TRUE) {
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
  if (export) {
    envir <- targets::tar_option_get("envir")
    parallel::clusterExport(
      cl = cluster,
      varlist = names(envir),
      envir = envir
    )
  }
  cluster
}
