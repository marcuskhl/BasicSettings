read.dem.sheets <- function(file_path,sheetnames){ 
  
  # this function is good for reading a large number of but not all sheets from a very big workbook
  #it returns a list of dataframes 
  
  #~~~~~~Values for Testing Start~~~~~~#
  
  # file_path <- "M:/Technology/DATA/STB_model/operator_box_requirements/DTH_requirements_.xlsx"
  # sheetnames <- c("Operators_data","Assumptions","churn_coefficient")
  #~~~~~~Values for Testing End~~~~~~#
  # lst = readWorksheet(wb, sheet = getSheets(wb))
  
  wb <- loadWorkbook(file = file_path)
  ldata <- list()
  for(i in 1:length(sheetnames)){
    ldata[[i]] <- read.xlsx(wb, sheet = sheetnames[i])
  }
  
  
  return(ldata)
  
}

get.dem.sheets <- function(list_df,sheetnames,sheetname){ 
  
  # this function reads a list of data frames and find the right one
  #it returns a list of dataframes 
  
  #~~~~~~Values for Testing Start~~~~~~#
  
  # file_path <- "M:/Technology/DATA/STB_model/operator_box_requirements/DTH_requirements_.xlsx"
  # sheetnames <- c("Operators_data","Assumptions","churn_coefficient")
  #~~~~~~Values for Testing End~~~~~~#
  # lst = readWorksheet(wb, sheet = getSheets(wb))
  
  x <-  list_df[[match(sheetname,sheetnames)]]
  
  
  return(x)
  
}
