#Functions Installer
as.df <- function(dt){
  as.data.frame(dt)
}

as.dt <- function(dt){
  as.data.table(dt)
}

#This function removes columns from a table
# pass in the df and the column and it will return a column without the mentioned column(s)
column.rm <- function(df,col_name){
  df <- df[,-match(col_name,names(df))]
  return(df)
}

print("Functions Loaded")


# This function converts factor column to numeric (usually applicable to year)
f2n <- function(col){
	return(as.numeric(as.character(col)))
}

# This function saves multiple dfs into one workbook
save.xlsx <- function (file, ...)
{
  wb <- createWorkbook()
  require(openxlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    addWorksheet(wb, sheetName = objnames[i])
    writeData(wb, objnames[i], objects[[i]])
  }
  saveWorkbook(wb, file, overwrite = T)
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}