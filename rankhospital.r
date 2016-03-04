source("helpers.R")
source("best.R")

## finds the num'th hospital in a given state for the requested outcome
## num="best" will return top ranked, num="worst" will return the bottom ranked
rankhospital <- function(state, outcome, num="best") {
        validateInputs(state, outcome)
        
        stateRankings <- getRankedDataForState(state, outcome)
        
        rankhospitalwithdata(stateRankings, num)
}

## returns the num'th hospital from the given set of ranked data
## num="best" will return top ranked, num="worst" will return the bottom ranked
rankhospitalwithdata <- function(stateRankings, num="best") {
        if (num == "best") {
                index <- 1
        }
        else if (num == "worst") {
                index <- NROW(stateRankings)
        }
        else {
                if (num <= NROW(stateRankings)) {
                        index <- as.numeric(num)
                } else {
                        return (NA)        
                }
        }
        
        as.character(stateRankings$Hospital[index])
}

