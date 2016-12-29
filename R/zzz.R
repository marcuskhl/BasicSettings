.onLoad <- function(libname, pkgname){
  list.of.packages <- c("XML", "rvest", "Hmisc", "MASS","RevoUtilsMath", "RevoUtils","sparkTable",
                        "rio", "psych","readr","data.table","magrittr","sparklyr",
                        "shiny","flexdashboard","gmodels","janitor",
                        "foreach","profvis","sqldf","reshape2","dplyr","zoo",
                        "scales","RODBC", "readxl","openxlsx","doParallel","dplyr","tidyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  update.packages(installed.packages()[,"Package"])
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, library, character.only = TRUE)

  cl <- makeCluster(4)
  registerDoParallel(cl)

  cat("\014")
  print("Libraries Loaded")

}
