library(dplyr)
library(tidyr)

datadirectory <- "specdata"

complete <- function(directory, id = 1:332) {
  
  ## directory is a character vector of length 1 indicating the location of
  ## the CSV files
  
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
  
  ## calculate no. complete cases and assign to data frame
  ## getwd() has been used to anonymise file path from my system for internet
  ## security - would just be file path if not being uploaded
  output <- data.frame()
  for(i in 1:length(id)) {
    var <- paste0(getwd(), "/", datadirectory, "/", id[i], ".csv")
    df <- read.csv(var)
    output[i, "id"] <- id[i]
    output[i, "nobs"] <- nrow(df[complete.cases(df),])
  }
  return(output)
}
