# parallel lapply over days
# CORES=1 runs one at a time

par_lapply <- function(x, fun, cores = as.integer(Sys.getenv("CORES", max(1, parallel::detectCores() - 1)))) {
  if (cores <= 1) return(lapply(x, fun))
  if (.Platform$OS.type != "windows") return(parallel::mclapply(x, fun, mc.cores = cores))

  # psock workers with the same packages and objects
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterCall(cl, \(lib, wd, pkgs) {
    .libPaths(lib)
    setwd(wd)
    for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
    NULL
  }, .libPaths(), getwd(), rev(.packages()))
  parallel::clusterExport(cl, ls(globalenv()), envir = globalenv())
  parallel::parLapply(cl, x, fun)
}
