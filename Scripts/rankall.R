rankall <- function(outcome, num = "best") {
        all_data <- read.csv("../Data/outcome-of-care-measures.csv")
        
        tc_outcome <- tools::toTitleCase(tolower(outcome))
        
        if (!(tc_outcome %in% c("Heart Attack", "Heart Failure", "Pneumonia"))) {
                stop("Invalid outcome: ")
        }
        
        outcome <- switch (tc_outcome, "Heart Attack" = 11, "Heart Failure" = 17, "Pneumonia" = 23)
        
        ## Coerce outcome rate to numeric 
        all_data[, outcome] <- suppressWarnings(as.numeric(all_data[, outcome]))
        
        ## Split by state and 
        out_state <- split(all_data[, c(2, 7, outcome)], all_data$State)
        
        ## Order by outcome and state
        sort_df <- lapply(out_state, function(x) x[order(x[[3]], x[[1]]), ])
        
        ## Complete cases 
        sort_cc <- lapply(sort_df, function(x) x[complete.cases(x), ])
        
        ## Add top to each state
        top_df <- lapply(sort_cc, function(x) cbind(x, Top = 1:nrow(x)))
        
        ## Create empty list
        hospitals_list <- vector(mode = "list")
        
        ##  For each state, find the hospital of the given rank
        for (i in seq_along(top_df)) {
                each_df <- top_df[[i]]
                
                rnum <- switch(tolower(as.character(num)), "best" = 1, "worst" = nrow(each_df), num)
                
                if (rnum > nrow(each_df))  
                        hospitals_list[[i]] <- data.frame(Hospital.Name = NA, State = unique(each_df$State))
                else 
                        hospitals_list[[i]] <- each_df[each_df$Top == rnum, 1:2]
        }
        
        ## Combine all dataframes in the hospitals list into one
        hospitals_df <- do.call(rbind, hospitals_list)
        
        ## Change column and rows names
        colnames(hospitals_df) <- c("hospital", "state")
        rownames(hospitals_df) <- hospitals_df$state
        
        ## Return a data frame with the hospital names and the state name
        hospitals_df
}

