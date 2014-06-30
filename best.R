
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

## Find the best hospital in a state
## based on the least number of
## heart attacks / failures and pneumonia
best <- function(state, outcome) {
  
  ## Read outcome data
  data = read.csv("./data/outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  if(!is.element(state, data$State)) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    outcome <- 11
  }
  else if (outcome == "heart failure") {
    outcome <- 17
  }
  else if (outcome == "pneumonia") {
    outcome <- 23
  }
  else {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest
  ## 30-day death rate
  d <- data[data$State==state,]
  d[, outcome] <- as.numeric(d[, outcome])
  name <- d[which.min(d[, outcome]),]$Hospital.Name
  return (name)
}

#print(best("TX", "heart attack"))