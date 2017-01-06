#' this function is short line to read multiple sheets in a df
#' @param file_path path of .xlsx files
#' @param sheetnames an array of sheet names you want to read
#' @return returns a list of data.frames of the sheets read
#' @export

read.dem.sheets <- function(file_path,sheetnames){

  wb <- loadWorkbook(file = file_path)
  ldata <- list()
  for(i in 1:length(sheetnames)){
    ldata[[i]] <- read.xlsx(wb, sheet = sheetnames[i])
  }

  return(ldata)

}

