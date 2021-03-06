#' installs necessary packages
packages.installer <- function(){
  list.of.packages <- c(
    # Web Scraping Tools:
    "XML", "rvest", "jsonlite", "rjson",
    # Misc Tools:
    "Hmisc", "MASS",
    # Web Analytics Tools:
    "googleAnalyticsR",
    # Data Manipulation Tools:
    "reshape2","dplyr","dplyr", "dtplyr", "tidyr", "janitor", "data.table","magrittr",# does not have reshape1
    # Modelling Tools:
    "psych", "gmodels", "caret", "ROCR", "cluster", "broom", "corrplot", "randomForest", "nnet", "forecast",
    # Multithreading:
    "foreach", "doParallel",
    # Input-Output:
    "sqldf", "RODBC", "readxl","openxlsx", "rio", "readr",
    # Misc:
    "zoo", "lubridate", "profvis", "scales", "RColorBrewer", "purrr", "httr"
  )

  update_and_install <- function(list.of.packages){
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    update.packages(installed.packages()[,"Package"])
    if(length(new.packages)) install.packages(new.packages, dependencies=T,
                                              lib = .libPaths()[1]
                                              )
    lapply(list.of.packages, require, character.only = TRUE)
  }
  update_and_install(list.of.packages)


  # cl <- makeCluster(4)  # might have to change it back to 2 for crappy laptops
  # registerDoParallel(cl)


  # For hardcore performance entheusiasts
  if(!is.na( match("Revo.version", ls(.BaseNamespaceEnv)))){
    list.of.packages <- c("RevoUtilsMath", "RevoUtils")
    update_and_install(list.of.packages)
    setMKLthreads(4)
  }

  cat("\014")
  print("Libraries Loaded")

}
