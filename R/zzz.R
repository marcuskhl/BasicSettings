.onLoad <- function(libname, pkgname){
  packages.installer()
  cl <- makeCluster(2)
  registerDoParallel(cl)
  options(digits=15)
  cat("\014")
<<<<<<< HEAD
  packageStartupMessage("Libraries Loaded\n")
  packageStartupMessage("Functions Loaded\n")
=======
>>>>>>> 685807d2771bcd6bbbe5d172410d031153dc7689
  # print("Libraries Loaded")
  # print("Functions Loaded")
}


.onAttach <- function(...) {
<<<<<<< HEAD

=======
  packageStartupMessage("Libraries Loaded\n")
  packageStartupMessage("Functions Loaded\n")
>>>>>>> 685807d2771bcd6bbbe5d172410d031153dc7689
}
