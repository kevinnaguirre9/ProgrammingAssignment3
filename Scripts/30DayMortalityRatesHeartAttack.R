## Read the outcome data into R via the read.csv function

outcome <- read.csv("../Data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

## Looking at the data

nrow(outcome)   
ncol(outcome)
names(outcome)
head(outcome)
tail(outcome)
summary(outcome)
str(outcome)


## Histogram of the 30-day death rates from heart attack  (col 11)

outcome[, 11] <- as.numeric(outcome[, 11])      ## coerce the column to be numeric
class(outcome[, 11])                            
hist(outcome[, 11], col = 2, main = "30-day death rates from heart attack", xlab = "rate")            

