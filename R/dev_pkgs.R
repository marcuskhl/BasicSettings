#' Seconday packages installer, used only by developers
#' @export
dev.pkgs <- function(){
  list.of.packages <- c(
    # Big Data Stuff:
    "sparkTable", "sparklyr", "Rfacebook", "twitteR", "RCurl", "httpuv", "httr", "DBI","boxr",
    # Rattle:
    "RGtk2","rattle",# problematic packages
    # Visualisation Tools:
    "shiny","flexdashboard", "htmlwidgets", "DT", "shinydashboard", "jsonlite","rsconnect", "RJSONIO",
    # Advanced Web Scraping:
    "RSelenium",
    # Visualisation Tools:
    "shiny","flexdashboard", "htmlwidgets", "DT", "shinydashboard", "rsconnect", "RJSONIO")

  update_and_install <- function(list.of.packages){
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    update.packages(installed.packages()[,"Package"])
    if(length(new.packages)) install.packages(new.packages,depen=T)
    lapply(list.of.packages, library, character.only = TRUE)
  }
  update_and_install(list.of.packages)
  cat("\014")
  print("dev pkgs installed")
  print("Welcome to the club")
}
