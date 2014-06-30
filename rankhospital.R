rankhospital <- function(state, outcome, num) {
  
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
  
  if (num == "worst") {
    dec = TRUE
  } else {
    dec = FALSE
  }
  
  if (num == "worst" || num == "best") {
    use_cat = TRUE
  } else {
    use_cat = FALSE
  }
  
  d <- data[data$State==state,]
  d[, outcome] <- as.numeric(d[, outcome])
  #d$Hospital.Name
  d <- d[order(d[, outcome], d$Hospital.Name, decreasing=dec),]
  
  if (use_cat) {
    d$Hospital.Name[1]
  } else {
    d$Hospital.Name[num]
  }
  
}

print(rankhospital("TX", "heart failure", 4))