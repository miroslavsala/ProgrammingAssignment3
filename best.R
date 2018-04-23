
best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% unique(df$State))) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  if (outcome == "heart attack") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    result <- subDf[subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE),]
    if (nrow(result) > 1) {
      result <- with(result,  result[order(result$Hospital.Name) , ])
    }
    return(result[1, 2])
    stop()
  }
  
  if (outcome == "heart failure") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    result <- subDf[subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm=TRUE),]
    if (nrow(result) > 1) {
      result <- with(result,  result[order(result$Hospital.Name) , ])
    }
    return(result[1, 2])
    stop()
  }
  
  if (outcome == "pneumonia") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    result <- subDf[subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm=TRUE),]
    if (nrow(result) > 1) {
      result <- with(result,  result[order(result$Hospital.Name) , ])
    }
    return(result[1, 2])
    stop()
  }
  stop()
}

# Testing of the function
debug(best)
best("TX", "heart attack") #Cypress
best("TX", "heart failure") #Fort Duncan
best("MD", "heart attack") #John Hopkins
best("MD", "pneumonia") #Greater Baltimore
best("BB", "heart attack") #error
best("NY", "hert attack") #error

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
