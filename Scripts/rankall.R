rankall <- function(outcome, num = "best") {
        all_data <- read.csv("../Data/outcome-of-care-measures.csv")
        
        out_format <- gsub(" ", ".", tools::toTitleCase(tolower(outcome)))
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", out_format, sep = "")
        
        if (!(outcome %in% names(all_data)[c(11, 17, 23)])) {
                stop("Invalid outcome")
        }
        
        ## Coerce outcome rate to numeric 
        all_data[, outcome] <- as.numeric(all_data[, outcome])
        
        ## Split by state
        out_state <- split(all_data[, c("Hospital.Name", "State", outcome)], all_data$State)
        
        ## Order by outcome and state
        sort_df <- lapply(out_state, function(x) x[order(x[[3]], x[[1]]), ])
        
        ## Complete cases 
        sort_cc <- lapply(sort_df, function(x) x[complete.cases(x), ])
        
        ## Add top to each state
        top_df <- lapply(sort_cc, function(x) cbind(x, Top = 1:nrow(x)))
        
        ## Create empty list
        hospitals_list <- vector(mode = "list")
        
        ## Getting the hospital in a specific top from a specific state 
        for (i in seq_along(top_df)) {
                each_df <- top_df[[i]]
                
                rnum <- if(tolower(num) == "best") {
                        1
                } else if (tolower(num) == "worst"){
                        nrow(each_df)
                } else {
                        num
                }
                
                if (rnum > nrow(each_df))  
                        hospitals_list[[i]] <- data.frame(Hospital.Name = NA, State = unique(each_df$State))
                else 
                        hospitals_list[[i]] <- each_df[each_df$Top == rnum, 1:2]
        }
        
        ## Cobine all dataframes in the hospitals list
        hospitals_df <- do.call(rbind, hospitals_list)
        
        ## Change column and rows names
        colnames(hospitals_df) <- c("hospital", "state")
        rownames(hospitals_df) <- hospitals_df$state
        
        ## return dataframe
        hospitals_df
}



