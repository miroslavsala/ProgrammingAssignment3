library(dplyr)

#is not finished!!! Must work on looping to add the NAs to the results

rankall <- function(outcome, num){
  
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  
  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack") {
    df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    df <- df[complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    df <- with(df, df[order(df$State, df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df$Hospital.Name),])
        ## Transform "num"
        if (num == "best") {
          num <- 1  
        }
        if (num == "worst") {
          num <- as.numeric(nrow(subDf))
        }
    by_state <- group_by(df, df$State)
    sliced <- slice(by_state, num)
    result <- data.frame(sliced$Hospital.Name, sliced$State)
    unique_t <- as.data.frame(unique(df$State))
    colnames(unique_t) <- "sliced.State"
    result2 <- merge(result, unique_t, all = TRUE)
    result2 <- with(result2, result2[order(result2$sliced.State),])
    colnames(result2) <- c("hospital","state")
    return(result)
    colnames(result2)
    stop()
  }
  
  if (outcome == "heart failure") {
    df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    df <- df[complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    df <- with(df, df[order(df$State, df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df$Hospital.Name),])
    ## Transform "num"
    if (num == "best") {
      num <- 1  
    }
    if (num == "worst") {
      num <- as.numeric(nrow(subDf))
    }
    by_state <- group_by(df, df$State)
    sliced <- slice(by_state, num)
    result <- data.frame(sliced$Hospital.Name, sliced$State)
    unique_t <- as.data.frame(unique(df$State))
    colnames(unique_t) <- "sliced.State"
    result2 <- merge(result, unique_t, all = TRUE)
    result2 <- with(result2, result2[order(result2$sliced.State),])
    colnames(result2) <- c("hospital","state")
    return(result)
    colnames(result2)
    stop()
  }
  
  if (outcome == "pneumonia") {
    df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    df <- df[complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    df <- with(df, df[order(df$State, df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df$Hospital.Name),])
    ## Transform "num"
    if (num == "best") {
      num <- 1  
    }
    if (num == "worst") {
      num <- as.numeric(nrow(subDf))
    }
    by_state <- group_by(df, df$State)
    sliced <- slice(by_state, num)
    result <- data.frame(sliced$Hospital.Name, sliced$State)
    unique_t <- as.data.frame(unique(df$State))
    colnames(unique_t) <- "sliced.State"
    result2 <- merge(result, unique_t, all = TRUE)
    result2 <- with(result2, result2[order(result2$sliced.State),])
    colnames(result2) <- c("hospital","state")
    return(result)
    colnames(result2)
    stop()
  }
}

### Testing

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

################ Others