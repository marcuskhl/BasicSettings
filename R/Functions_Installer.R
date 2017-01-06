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



# This function converts factor column to numeric (usually applicable to year)
f2n <- function(col){
	return(as.numeric(as.character(col)))
}



# This function converts factor column to char
f2c <- function(col){
  return(as.character(col))
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



# This function changes multiple colnames of a df there is a range match option, just like in Excel match(,,1)
# range_match True test data set
# df <- flat_data
# original_names <- c("households", "Installed")
# new_names <- c("TV.households","Installed.base")
# range_match = T
# range_match False test data set
# df <- mtcars
# original_names <- c("mpg","disp","wt")
# new_names <- c("test1","test2","test3")
# range_match = F
df.name.change <- function(df, original_names, new_names, range_match = F){
  fn_flag <- F
  if (class(df)[1]==1){
    df <- as.df(df)
    fn_flag <- T
  }
  if (range_match){
    if(length(grep(paste(original_names,collapse="|"), names(df), value = T))!= length(new_names)){
      print("# of matched names not equal to # of new names")
    }
    names(df)[match(grep(paste(original_names,collapse="|"), names(df), value = T), names(df))] <- new_names
  }else{
    if(length(match(original_names, names(df)))!= length(new_names)){
      print("# of matched names not equal to # of new names")
    }
    names(df)[match(original_names, names(df))] <- new_names
  }
  if(fn_flag){return(as.dt(df))}else{return(df)}
}

# flat_data <- df.name.change(flat_data, c("households", "Installed", "Domestic", "Shipments", "Revenues"), 
#                             c("TV.households", "Installed.base", "Domestic.consumption", "Shipments", "Revenues"), T)
# The above equals below:
# names(flat_data)[grep("households", names(flat_data))] <- "TV.households"
# names(flat_data)[grep("Installed", names(flat_data))] <- "Installed.base"
# names(flat_data)[grep("Domestic", names(flat_data))] <- "Domestic.consumption"
# names(flat_data)[grep("Shipments", names(flat_data))] <- "Shipments"
# names(flat_data)[grep("Revenues", names(flat_data))] <- "Revenues"







print("Functions Loaded")
