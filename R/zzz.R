.onLoad <- function(libname, pkgname){
  packages.installer()
  cl <- makeCluster(2)
  registerDoParallel(cl)
  options(digits=15)
  cat("\014")
  # print("Libraries Loaded")
  # print("Functions Loaded")
}


.onAttach <- function(...) {
  packageStartupMessage("Libraries Loaded\n")
  packageStartupMessage("Functions Loaded\n")
}
