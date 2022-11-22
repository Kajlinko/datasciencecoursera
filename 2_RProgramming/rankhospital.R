rankhospital <- function(state, outcome, num = "best") {
  ## state should be two-character abbreviated state name
  ## outcome is a character string specifying the outcome of interest
  ## outcome options are heart attack, heart failure, pneumonia
  ## num should be character vector "best" or "worst", or an integer
  ## specifying the rank you wish to check
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- as.vector(unique(data$State))
  if(is.na(charmatch(state, states))) {
    stop("invalid state")
  } 
  
  if(is.na(charmatch(outcome, c("heart attack","heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  ## Set ranking columns as numeric and set num value
  
  library(dplyr)
  data_state <- data %>%
    filter(State == state)
  
  data_state[,11] <- as.numeric(data_state[,11])
  data_state[,17] <- as.numeric(data_state[,17])
  data_state[,23] <- as.numeric(data_state[,23])
  
  if(num == "best") {
    num <- 1
  } else if(num == "worst") {
    ## Set num value
    if(outcome == "heart attack") {
      dat_squash <- data_state[!is.na(data_state[,11]),]
      num <- nrow(dat_squash)
    } else if(outcome == "heart failure") {
      dat_squash <- data_state[!is.na(data_state[,17]),]
      num <- nrow(dat_squash)
    } else if(outcome == "pneumonia") {
      dat_squash <- data_state[!is.na(data_state[,23]),]
      num <- nrow(dat_squash)
    } else {
      num <- num
    }
  }
  
  if(num > nrow(data_state)) {
    print(NA)
  } else {
    
    if(outcome == "heart attack") {
      data_state <- data_state %>%
        arrange(
          data_state[,11],
          data_state[,2]
        )
      print(data_state[num, 2])
      
      
    } else if(outcome == "heart failure") {
      data_state <- data_state %>%
        arrange(
          data_state[,17],
          data_state[,2]
        )
      print(data_state[num, 2])
      
    } else if(outcome == "pneumonia") {
      data_state <- data_state %>%
        arrange(
          data_state[,23],
          data_state[,2]
        )
      print(data_state[num, 2])
    }
  }
}