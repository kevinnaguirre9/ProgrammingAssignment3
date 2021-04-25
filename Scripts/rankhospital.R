rankhospital <- function(state, outcome, num) {
        ## Read outcome data
        outcome_data <- read.csv("../Data/outcome-of-care-measures.csv")
        
        ## Uppercase and format spaces in outcome argument
        cap_outcome <- gsub(" ", ".", tools::toTitleCase(tolower(outcome)))
        ## Get full name of outcome 
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", cap_outcome, sep = "")
        state <- toupper(state)  ## convert state argument to uppercase
        
        ## Check that state and outcome are valid
        if (!(outcome %in% names(outcome_data)[c(11, 17, 23)])) 
                stop("Invalid outcome")
        else if (!(state %in% unique(outcome_data$State)))
                stop("Invalid state")
        
        ## Dataset with a specific outcome from a specific state
        outcome_state <- outcome_data[outcome_data$State == state, c("Hospital.Name", outcome)]
        
        ## Coercing outcome to numeric
        outcome_state[, 2] <- suppressWarnings(as.numeric(outcome_state[, 2]))
        
        ## Get complete cases
        out_state_cc <- outcome_state[complete.cases(outcome_state), ]
        
        ## Sorting by rate and hospital (asc) and adding 'top' column
        tidy_data <- out_state_cc[order(out_state_cc[[2]], out_state_cc[[1]]), ]
        top_data <-cbind(tidy_data, top = 1:nrow(tidy_data))
        
        ## Setting value of 'num' argument 
        nrank <- if (tolower(num) == "best") {
                        1
                } else if (tolower(num) == "worst") {
                        nrow(top_data)
                } else {
                        num
                }
        
        ## return NULL if top doesn't exists
        if (nrank > nrow(top_data)) return(NA)
        
        ## otherwise return hospital in that rank
        top_data[top_data$top == nrank, "Hospital.Name"]
}

