.onLoad <- function(libname, pkgname){
  list.of.packages <- c("XML", "rvest", "Hmisc", "MASS",
                        # "RevoUtilsMath", "RevoUtils",
                        "sparkTable",
                        "reshape2","dplyr","reshape",
                        "rio", "psych","readr","data.table","magrittr","sparklyr", "Rfacebook","RColorBrewer","RCurl","httpuv","httr","rjson",
                        "shiny","flexdashboard","gmodels","janitor", "caret","ROCR",
                        "foreach","profvis","sqldf","zoo",
                        "scales","RODBC", "readxl","openxlsx","doParallel","tidyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  update.packages(installed.packages()[,"Package"])
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, library, character.only = TRUE)

  cl <- makeCluster(2)
  registerDoParallel(cl)

  cat("\014")
  print("Libraries Loaded")

}
