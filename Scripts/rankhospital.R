rankhospital <- function(state, outcome, num) {
        ## Read outcome data
        outcome_data <- read.csv("../Data/outcome-of-care-measures.csv")
        
        ## Uppercase and format spaces in outcome argument
        cap_outcome <- gsub(" ", ".", tools::toTitleCase(tolower(outcome)))
        ## Get full name of outcome 
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", cap_outcome, sep = "")
        state <- toupper(state)  ## convert state argument to uppercase
        
        ## Names of valid outcomes (outcomes in the dataset)
        outcome_names <- names(outcome_data)[c(11, 17, 23)]
        ## Get states from the dataset
        uu_states <- unique(outcome_data$State)
        
        ## Check that state and outcome are valid
        if (!(outcome %in% outcome_names)) 
                stop("Invalid outcome")
        else if (!(state %in% uu_states))
                stop("Invalid state")
        
        ## dataset with a specific outcome from a specific state
        outcome_state <- outcome_data[outcome_data$State == state, c("Hospital.Name", outcome)]
        
        ## coercing outcome to numeric
        outcome_state[, 2] <- as.numeric(outcome_state[, 2]) 
        
        ## Sorting by rate and hospital (ascending and nulls last by default)
        top <- 1:nrow(outcome_state)
        rank_data <- outcome_state[order(outcome_state[[2]], outcome_state[[1]]), ]
        top_data <-cbind(rank_data, top)
        
        ## Get complete cases
        rank_data_cc <- top_data[complete.cases(top_data), ]
        
        ## num argument values
        nrank <- if (tolower(num) == "best") {
                        1
                } else if (tolower(num) == "worst") {
                        nrow(rank_data_cc)
                } else {
                        num
                }
        
        ## If top does not exists, return NULL
        if (nrank > nrow(rank_data_cc))
                return(NULL)
        
        ## otherwise return hospital in that rank
        rank_data_cc[rank_data_cc$top == nrank, "Hospital.Name"]
}

