
rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state, outcome, and num are valid
  if (!(state %in% unique(df$State))) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  if (is.integer(num)) {
    if (num > nrow(df[df$State == state,])) {
      stop("NA")  
    }
  } 
  

  ## Return hospital name in that state with lowest 30-day death rate
  if (outcome == "heart attack") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    subDf <- subDf[complete.cases(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    subDf <- with(subDf, subDf[order(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, subDf$Hospital.Name),])
        ## Transform "num"
        if (num == "best") {
          num <- 1  
        }
        if (num == "worst") {
          num <- as.numeric(nrow(subDf))
        }
    result <- subDf[num, 2]
    return(result)
    stop()
  }
  
  if (outcome == "heart failure") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    subDf <- subDf[complete.cases(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    subDf <- with(subDf, subDf[order(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, subDf$Hospital.Name),])
    ## Transform "num"
    if (num == "best") {
      num <- 1  
    }
    if (num == "worst") {
      num <- as.numeric(nrow(subDf))
    } 
    result <- subDf[num, 2]
    return(result)
    stop()
  }
  
  if (outcome == "pneumonia") {
    subDf <- df[df$State == state,]
    subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    subDf <- subDf[complete.cases(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    subDf <- with(subDf, subDf[order(subDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, subDf$Hospital.Name),])
    ## Transform "num"
    if (num == "best") {
      num <- 1  
    }
    if (num == "worst") {
      num <- as.numeric(nrow(subDf))
    }
    result <- subDf[num, 2]
    return(result)
    stop()
  }
  stop()

}

# Testing of the function
debug(rankhospital)

rankhospital("TX", "heart failure", 4)      # Detar
rankhospital("MD", "heart attack", "worst") # Harford
rankhospital("MN", "heart attack", 5000)    # NA

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

rankhospital("HI", "heart attack", 4)
rankhospital("NJ", "pneumonia", "worst")
rankhospital("NV", "heart failure", 10)
