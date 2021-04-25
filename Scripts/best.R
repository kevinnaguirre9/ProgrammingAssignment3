best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <- read.csv("../Data/outcome-of-care-measures.csv")
        
        ## Title case and format spaces in outcome argument
        cap_outcome <- gsub(" ", ".", tools::toTitleCase(tolower(outcome)))
        ## Get full name of outcome 
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", cap_outcome, sep = "")
        state <- toupper(state)  ## convert state argument to uppercase

        ## Check that state and outcome are valid
        if (!(outcome %in% names(outcome_data)[c(11, 17, 23)])) 
                stop("Invalid outcome")
        else if (!(state %in% unique(outcome_data$State)))
                stop("Invalid state")
        
        ## Creating dataset with a specific outcome from a specific state
        state_outcome <- outcome_data[outcome_data$State == state, c("Hospital.Name", outcome)]
        
        ## Coercing outcome to numeric
        state_outcome[, 2] <- suppressWarnings(as.numeric(state_outcome[, 2]))
        
        ## Getting complete cases of the dataset 
        state_outcome_cc <- state_outcome[complete.cases(state_outcome), ]
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## If there is a tie, then the first hospital is returned (alphabetical order)
        hospital <- state_outcome_cc[state_outcome_cc[[2]] == min(state_outcome_cc[[2]]), 1]
        sort(hospital)[1]
} 
