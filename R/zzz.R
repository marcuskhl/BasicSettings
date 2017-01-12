.onLoad <- function(libname, pkgname){
  packages.installer()
  cl <- makeCluster(4)
  registerDoParallel(cl)
  options(digits=15)
  cat("\014")
}


.onAttach <- function(...) {
  packageStartupMessage("Libraries Loaded\n")
  packageStartupMessage("Functions Loaded\n")
  packageStartupMessage("Keep Calm and Drink Basket Pressed Shiraz :)\n")
}
