best <- function(state, outcome) {
  ## state should be two-character abbreviated state name
  ## outcome is a character string specifying the outcome of interest
  ## outcome options are heart attack, heart failure, pneumonia
  
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  library(dplyr)
  data_state <- data %>%
    filter(State == state)
  
  data_state[,11] <- as.numeric(data_state[,11])
  data_state[,17] <- as.numeric(data_state[,17])
  data_state[,23] <- as.numeric(data_state[,23])
  
  if(outcome == "heart attack") {
    data_state <- data_state %>%
      arrange(
        data_state[,11],
        data_state[,2]
      )
    print(data_state[1,2])
  } else if(outcome == "heart failure") {
    data_state <- data_state %>%
      arrange(
        data_state[,17],
        data_state[,2]
      )
    print(data_state[1,2])
  } else if(outcome == "pneumonia") {
    data_state <- data_state %>%
      arrange(
        data_state[,23],
        data_state[,2]
      )
    print(data_state[1,2])
  }
}
