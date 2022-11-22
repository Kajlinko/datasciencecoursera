library(dplyr)
library(tidyr)

datadirectory <- "specdata"

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {

## directory is a character vector of length 1 indicating the location of
## the CSV files
  
## pollutant is a character vector of length 1 either 'sulfate' (default)
## or 'nitrate'
  
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
  vect <- vector("numeric", length(id))
  dat <- data.frame()
  for(i in 1:length(id)) {
    var <- paste0(getwd(), "/", directory, "/", id[i], ".csv")
    df <- read.csv(var)
    vect[i] <- df %>%
      select(pollutant) %>% drop_na() %>% unlist() %>% mean()
    dat <- rbind(dat, df %>% drop_na())
  }
  dat[,pollutant] %>% unlist %>% mean %>% print
  print(mean(vect))
}

pollutantmean("specdata", "nitrate", 70:72)
