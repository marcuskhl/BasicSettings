% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_Installer.R
\name{df.name.change}
\alias{df.name.change}
\title{This function changes multiple colnames of a df there is a range match option}
\usage{
df.name.change(df, original_names, new_names, range_match = F, fixed = F,
  invert = F)
}
\arguments{
\item{df}{a data.frame}

\item{original_names}{a colname/ an array of colnames you want to change}

\item{new_names}{a target colname/ an array of target colnames, must match the number of names in original_names}

\item{range_match}{just like in Excel match(,,1), default is False}

\item{fixed:}{only applicable if range_match = T, used in grep, please see grep for purpose}

\item{invert:}{only applicable if range_match = T, used in grep, please see grep for purpose, essentially making original_names columns you do not want to change name}
}
\description{
This function changes multiple colnames of a df there is a range match option
}
\examples{
range_match True test data set
df <- flat_data
original_names <- c("households", "Installed")
new_names <- c("TV.households","Installed.base")
range_match = T
range_match False test data set
df <- mtcars
original_names <- c("mpg","disp","wt")
new_names <- c("test1","test2","test3")
range_match = F
}
