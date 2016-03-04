source("best.r")
source("rankhospital.r")

rankall <- function(outcome, num="best") {
        if (!isOutcomeValid(outcome)) {
                stop("invalid outcome")
        }
        
        data <- getRelevantData(outcome)
        
        sortedStates <- sort(getStates())
        
        hospitals <- vector(mode="character", length=length(sortedStates))
        
        i <- 1
        for(state in sortedStates) {
                stateRankings <- filterRankedDataForState(data, state)

                ret <- rankhospitalwithdata(stateRankings, num)
                
                hospitals[i] <- ret
                
                i <- i+1
        }
        
        data.frame(state=sortedStates, hospital=hospitals)
}