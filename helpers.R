stateCache <- NULL

## returns a data frame of data read from the outcome-of-care-measures.csv file
getData <- function() {
        read.csv("outcome-of-care-measures.csv", colClasses="character")        
}

## returns a data frame of hospital outcome data for the given outcome
getRelevantData <- function (outcome) {
        outcomeColName <- getOutcomeColumn(outcome)
        patientColName <- getPatientCountColumn(outcome)
        
        data <- getData()
        
        hospitalNames <- data$Hospital.Name
        states <- data$State
        outcomes <- data[outcomeColName]
        patientCounts <- data[patientColName]
        
        ## build the subset data frame
        ret <- data.frame(state=states, hospital=hospitalNames, outcome=outcomes, count=patientCounts)
        names(ret) <- c("State", "Hospital", "Outcome", "Count")
        
        ## filter out data points where data is not available
        filtered <- ret[as.character(ret$Outcome) != "Not Available" & as.character(ret$Count) != "Not Available",]
        
        ## convert outcome and count columns to numeric
        filtered$Outcome <- sapply(filtered$Outcome, as.numeric)
        filtered$Count <- sapply(filtered$Count, as.numeric)
        
        ## return the filtered data
        filtered
}

getStates <- function() {
        if (is.null(stateCache)) {
                data <- getData()
                states <- unique(as.character(data$State))
        }
        states
}

filterRankedDataForState <- function(data, state) {
        filtered <- data[data$State == state,]
        
        filtered[order(filtered$Outcome, filtered$Hospital),]
}

## returns filtered data for the specified state, sorted by outcome then hospital name
getRankedDataForState <- function(state, outcome) {
        data <- getRelevantData(outcome)
        
        filterRankedDataForState(data, state)
}

validateInputs <- function(state, outcome) {
        if (!isStateValid(state)) {
                stop("invalid state")
        }
        if (!isOutcomeValid(outcome)) {
                stop("invalid outcome")
        }
}

## returns the formatted name of the specified outcome
getFormattedOutcomeName <- function(name) {
        if (name == "heart attack") {
                return("Heart.Attack")
        } else if (name == "heart failure") {
                return("Heart.Failure")
        } else if (name == "pneumonia") {
                return("Pneumonia")
        } else {
                return(NA)
        }
}

isOutcomeValid <- function(outcome) {
        name <- getFormattedOutcomeName(outcome)

        !is.na(name)
}

isStateValid <- function(stateCode) {
        stateCode %in% getStates()        
}

## returns the formatted outcome column name for the specified outcome
getOutcomeColumn <- function(outcome) {
        outcomeName <- getFormattedOutcomeName(outcome)
        
        paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcomeName, sep="")
}

## returns the formatted patient count column name for the specified outcome
getPatientCountColumn <- function(outcome) {
        outcomeName <- getFormattedOutcomeName(outcome)
        
        paste("Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.", outcomeName, sep="")
}

