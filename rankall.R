## setwd("D:/Projekte/Coursera.org/R Programming/ProgrammingAssignment3")

rankall <- function(outcome, num) {
  
  ## Read outcome data
  data = read.csv("./data/outcome-of-care-measures.csv", colClasses="character")
  
  
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
  
  states <- levels(factor(data$State))
  #out <- data.frame()
  out = matrix(nrow=length(states), ncol=2)
  
  data[, outcome] <- as.numeric(data[, outcome])
  
  i <- 1
  for (s in states) {
    d <- data[data$State==s,]
    d <- d[order(d[, outcome], d$Hospital.Name, decreasing=dec),]
    if (use_cat) {
      out[i,] = c(d$Hospital.Name[1], d$State[1])
      
    } else {
      out[i,] = c(d$Hospital.Name[num], d$State[num])
    }
    i <- i + 1
  }
  
  out <- as.data.frame(out)
  colnames(out) = c("hospital", "state")
  
  return (out)
  
}

print(tail(rankall("pneumonia", "worst"), 3))