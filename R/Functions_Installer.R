#Handy Functions Installer

#' shorter version of as.data.frame
#' @param dt an object of class data.table
#' @export
as.df <- function(dt){
  as.data.frame(dt)
}


#' shorter version of as.data.table
#' @param df an object of class data.table
#' @export
as.dt <- function(df){
  as.data.table(df)
}



#' This function removes columns from a table
#' pass in the df and the column and it will return a column without the mentioned column(s)
#' @param df a data frame
#' @param col_name 1 or multiple concatenated column names
#' @return returns the data frame without the specified columns
#' @export
column.rm <- function(df,col_name){
  df <- df[,-match(col_name,names(df))]
  return(df)
}



#' This function converts factor column to numeric (usually applicable to year)
#' @param col the column in a data frame
#' @examples
#' x$abc <- f2n(x$abc)
#' @export
f2n <- function(col){
	return(as.numeric(as.character(col)))
}



#' This function converts factor column to char
#' #' @param col the column in a data frame
#' @examples
#' x$abc <- f2c(x$abc)
#' @export
f2c <- function(col){
  return(as.character(col))
}


local({
     r <- getOption("repos")
     r["CRAN"] <- "http://cran.rstudio.com"
     options(repos = r)
})
#' This function saves multiple dfs into one workbook
#' The sheet names will be the variable names
#' @param file file path you wish to write to
#' @param ... list of dfs you wish to write
#' @export
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



#' This function changes multiple colnames of a df there is a range match option
#' @param df a data.frame
#' @param original_names a colname/ an array of colnames you want to change
#' @param new_names a target colname/ an array of target colnames, must match the number of names in original_names
#' @param range_match just like in Excel match(,,1), default is False
#' @param fixed: only applicable if range_match = T, used in grep, please see grep for purpose
#' @param invert: only applicable if range_match = T, used in grep, please see grep for purpose, essentially making original_names columns you do not want to change name
#' @examples
#' range_match True test data set
#' df <- flat_data
#' original_names <- c("households", "Installed")
#' new_names <- c("TV.households","Installed.base")
#' range_match = T
#' range_match False test data set
#' df <- mtcars
#' original_names <- c("mpg","disp","wt")
#' new_names <- c("test1","test2","test3")
#' range_match = F
#' @export
df.name.change <- function (df, original_names, new_names, range_match = F, fixed = F, invert = F) {
  fn_flag <- F
  if (class(df)[1] == 1) {
    df <- as.df(df)
    fn_flag <- T
  }
  if (range_match) {
    if (invert) {
      names(df)[match(grep(paste(original_names, collapse = "|"),
                           names(df),
                           value = T,
                           fixed = fixed, invert = invert),
                      names(df))] <- new_names
    }
    else {
      if (length(grep(paste(original_names, collapse = "|"),
                      names(df), value = T, fixed = fixed, invert = invert)) != length(new_names)) {
        print("#' of matched names not equal to #' of new names")
      }
      for (i in 1: (length(original_names))){
        names(df)[grep(original_names[i], names(df), fixed = T)] <- new_names[i]
      }
      # This yields the wrong order
      # names(df)[match(grep(paste(original_names, collapse = "|"),
      #                      names(df),
      #                      value = T,
      #                      fixed = fixed, invert = invert),
      #                 names(df))] <- new_names
    }
  }
  else {
    if (length(match(original_names, names(df))) != length(new_names)) {
      print("#' of matched names not equal to #' of new names")
    }
    names(df)[match(original_names, names(df))] <- new_names
  }
  if (fn_flag) {
    return(as.dt(df))
  }
  else {
    return(df)
  }
}

#' This is a piper for functions
#' @param ... the functions you want to pipe
#' @return returns a custom function that executes all the functions in reversed order
#' @examples
#' #' custom_fn <- fn.piper(length, unique)
#' custom_fn(rep(1:5, 1:5)) would be 5
#'
#' custom_fn <- fn.piper(is.numeric, length, unique)
#' custom_fn(rep(1:5, 1:5)) would be TRUE
#'
#' @export
fn.piper <- function(...){
  return(compose(...))
}


#' This function sources scripts from git_hub
#' @param url urls
#' @examples
#' source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R",
#' "https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/htmlToText/htmlToText.R")
#' @export
source_https <- function(url, ...) {
  # load package
  require(RCurl)

  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}



#' Sometimes when reading in < and >, it gets encoded differently, this function fixes it
#' @param temp_col column to perform this operation
#' @export
size.encoder <- function(temp_col){
  temp_col <- gsub("&gt;", ">",temp_col, fixed = T)
  temp_col <- gsub("&lt;", "<",temp_col, fixed = T)
  return(temp_col)
}



#' When read in CSVs using fread, " usually being read in as "", this function fixes it
#' @param temp_col column to perform this operation
#' @export
dt.double_quote_fix <- function(temp_col){
  temp_col <- gsub("\"\"","\"",temp_col, fixed = T)
}


#' This function generates size range
#' @param df df you want to generate size cat
#' @param col_name Size column name of in df you want to generate range for
#' @param range the window of size range, eg. 5 means say 10"-15" & 15"-20"
#' @export
size_cat_gen <- function(df, col_name ,range){
  if(class(df$`Screen Size`)!= "numeric"){print("worng class")}else{
    col <- df$`Screen Size`
    lower <- floor(col/range)*range
    upper <- ceiling(col/range)*range -1 # have error, next line is the patch
    upper[floor(col/range)*range==ceiling(col/range)*range] <- upper[floor(col/range)*range==ceiling(col/range)*range] +range
    new_col <- paste0(lower, "\"-", upper,  "\"")
    new_col <- as.df(new_col)
    names(new_col) <- paste0("Size Group (", range, "-inch)")
    return(cbind.data.frame(df, new_col))
  }
}

#' This function is mainly for internal use so I wont export it right now
#' Checks if the table is a df or a dt
#' @param df a data.frame or data.table
fn.is.dt.start <- function(df){
  dt_flag <<- F
  if (class(df)[1] == 1) {
    df <- as.df(df)
    dt_flag <<- T
  }
  return(df)
}

#' complimentary to the function above
#' @param dt_flag is a variable produced in the function above
fn.is.dt.end <- function(df, dt_flag){
  if(dt_flag){
    return(as.dt(df))
  }else{
    return(df)
  }
}



#' This function converts the character columns into factor, useful in visualising correlation matrix
#' @export
df.c2f <- function(df){
  df <- fn.is.dt.start(df)
  for(i in 1:dim(df)[2]){
    if(class(df[,i])=="character"){
      df[,i] <- as.factor(df[,i])
    }
  }
  return(fn.is.dt.end(df,dt_flag))
}



#' This function extracts only columns references that are either numeric, character or factor
#' @param class_type either "numeric", "character" or "factor"
#' @export
df.col.select <- function(df, class_type){
  temp_function <- get(paste0("is.", class_type))
  return(sapply(df, temp_function))
}



#' This function extracts only columns that are either numeric, character or factor
#' @param class_type either "numeric", "character" or "factor"
#' @export
df.class.extract <- function(df, class_type){
  df <- fn.is.dt.start(df)
  if(Reduce("|", grepl(class_type, c("numeric", "character", "factor")))){
    temp_list <- df.col.select(df, class_type)
    new_df <-df[ , temp_list]
    return(fn.is.dt.end(new_df,dt_flag))
  }else{
    print("Unsupported Type")
  }
}

#' @export BasicSettings
