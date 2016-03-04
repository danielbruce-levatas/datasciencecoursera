plotHistogram <- function() {
        outcome <- getData()
        outcome[, 11] <- as.numeric(outcome[,11])
        hist(outcome[,11])
}

## finds the best hospital in a given state for the requested outcome
best <- function(state, outcome) {
        validateInputs(state, outcome)
        
        stateRankings <- getRankedDataForState(state, outcome)
        
        as.character((stateRankings$Hospital)[1])
}

## finds the worst hospital in a given state for the requested outcome
worst <- function(state, outcome) {
        validateInputs(state, outcome)
        
        stateRankings <- getRankedDataForState(state, outcome)
        
        as.character((stateRankings$Hospital)[length(stateRankings$Hospital)])
}
