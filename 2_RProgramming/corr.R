library(dplyr)
library(tidyr)

datadirectory <- "specdata"

corr <- function(directory, threshold = 0, id = 1:332) {
  
  ## directory is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## threshold is an integer vector of length 1 indicating the number of 
  ## complete cases required to compute the correlation
  
  ## id is an integer vector containing the numeric code for the monitor 
  ## id numbers to be used
  
  ## make monitor ids into three digit character strings
  
  for(i in 1:length(id)) {
    if(nchar(id[i]) == 1) {
      id[i] <- paste0("00",id[i])
    } else if (nchar(id[i]) == 2) {
      id[i] <- paste0("0",id[i])
    } else {
      id[i] <- as.character(id[i])
    }
  }
  
  ## calculate mean of each monitor's data, calculate mean of summed monitor
  ## data
  ## getwd() has been used to anonymise file path from my system for internet
  ## security - would just be file path if not being uploaded
  
  vect <- vector("numeric", 0)
  for(i in 1:length(id)) {
    string <- paste0(getwd(), "/", datadirectory, "/", id[i], ".csv")
    df <- read.csv(string)
    if(nrow(df[complete.cases(df),]) > threshold) {
    var <- cor(df$sulfate, df$nitrate, use = "complete.obs")
    vect <- c(vect, var)
    } else if(nrow(df[complete.cases(df),]) <= threshold) {
      vect <- c(vect, NA)
    }
  } 
  return(vect)
}
