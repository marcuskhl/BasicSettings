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