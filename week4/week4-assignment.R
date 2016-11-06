# R Programming
# Week 4 Assignment
# Victor Otero Cela
# November 2016

week4.setup <- function() {
    
    ## Downloads files and sets up working environment
    
    data_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
    working_dir <- "~/Coursera/r-programming/week4/"
    destFile <- "week4-data.zip"
    
    setwd(working_dir)
    
    if(!file.exists(destFile)) {
        download.file(data_url, destfile = destFile, method="curl")
        dateDownloaded<-date()
        dateDownloaded
        unzip(destFile, exdir = "./")
    }
    
    print("All files downloaded and available to work on.")
    list.files(working_dir)
}

# Exercise 1: Plot the 30-day mortality rates for heart attack
week4.ex1 <- function() {
    
    outcome_file <- "outcome-of-care-measures.csv"
    
    if(file.exists(outcome_file)) {
        outcome <-read.csv(outcome_file, colClasses = "character")
        outcome[,11] <- as.numeric(outcome[,11])
        hist(outcome[, 11])
        
    } else {
        print("Error: File not found.")
        return(1)
    }
}

# Exercise 2: Finding the best hospital in a state
# Input: State Abbreviation and outcome name
# Output: Character Vector with the hospital with best mortality rate for
#         that outcome in that state.
best <- function(state, outcome) {
    
    ## Read Outcome Data
    
    outcome_file <- "outcome-of-care-measures.csv"
    
    if(file.exists(outcome_file)) {
        DF <-read.csv(outcome_file, na.strings="Not Available", stringsAsFactors = FALSE)
        
    } else {
        print("Error: File not found.")
        stop()
    }
    
    ## Check that state and outcome are valid
    ## Create a list of states and check if variable is inside
    ## Create a list of outcomes and check if variables is included
    # heart attack [,11] | heart failure [,17] | pneumonia [, 23]
    
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    states <- DF[,7]
   
    if (!(outcome %in% names(outcomes))) { 
        print("Error: Outcome not found in table. Please check again.")
        stop()
    }
    
    if (!(state %in% states)) {
        print("Error: State not found in table. Please check again.")
        stop()
    }
    
    ## Subsetting outcome colum, with Hospital Name [,2] and State [,7]
    DF2 <- DF[, c(2,7, outcomes[outcome])]
    DF3 <- DF2[DF2$State== state, ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## Hospital Name in column #2 (Hospital.Name)
    
    DF4 <- DF3[order(DF3[,3]),]
    
    DF4$Hospital.Name[[1]]
}

## Exercise 3 Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    
    ## Read Outcome Data
    outcome_file <- "outcome-of-care-measures.csv"
    if(file.exists(outcome_file)) {
        DF <-read.csv(outcome_file, na.strings="Not Available", stringsAsFactors = FALSE)
    } else {
        print("Error: File not found.")
        stop()
    }
    ## Check that state and outcome are valid
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (!(outcome %in% names(outcomes))) { 
        print("Error: Outcome not found in table. Please check again.")
        stop()
    }
    if (!(state %in% DF[,7])) {
        print("Error: State not found in table. Please check again.")
        stop()
    }
    ## Subsetting outcome colum, with Hospital Name [,2] and State [,7]
    DF2 <- DF[, c(2,7, outcomes[outcome])]
    DF3 <- DF2[DF2$State== state, ]
    # Ranking order supporting words
    ## Return hospital name in that state with lowest 30-day death rate
    ## Sort Data Frame by Outcome [,3] and then by name [, 1]
    DF4 <- na.omit(DF3[order(DF3[,3], DF3[,1]),])
    order <- c("best" = 1, "worst" = nrow(DF4))
    
    if (num %in% names(order)) {
        DF4$Hospital.Name[[order[num]]]
    } else if(num > order["worst"]) {
           return(NA)
    } else {
        DF4$Hospital.Name[[num]]
    }
    
}

## Exercise 4 Ranking hospitals in all states
## Returns a two column data frame containing the names of the hospitals that are
## in that rank for the respective state. It should return a value for every state.
## First column is hospital with the hospital name, and second state, with the state
## character.
rankall <- function(outcome, num = "best") {
    ## Read Outcome Data
    outcome_file <- "outcome-of-care-measures.csv"
    if(file.exists(outcome_file)) {
        DF <-read.csv(outcome_file, na.strings="Not Available", stringsAsFactors = FALSE)
    } else {
        print("Error: File not found.")
        stop()
    }
    ## Check that state and outcome are valid
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (!(outcome %in% names(outcomes))) { 
        print("Error: Outcome not found in table. Please check again.")
        stop()
    }
    ## Subsetting outcome colum, with Hospital Name [,2] and State [,7]
    DF2 <- DF[, c(2,7, outcomes[outcome])]
    ## Sort Data Frame by State, then OutCome, then name
    DF3 <- na.omit(DF2[order(DF2[,2], DF2[,3], DF2[,1]),])
    
    ## For each state, find the hospital of the given rank
    DF4 <- split(DF3, DF3$State)
    DF5 <- lapply(DF4, function(DFX) {
        order <- c("best" = 1, "worst" = nrow(DFX))
        if (num %in% names(order)) {
              DFX[order[num],c(1,2)]
        } else {
            DFX[num,c(1,2)]
        }
        
    })
    ## Return a data frame with the hospital names and the state name
    do.call(rbind, DF5)
    #data.frame(hospital = unlist(DF5), state=names(DF5))
    #names(DF5)
}