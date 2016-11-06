pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## directory is a character vector of length 1 indidcating the location of 
    ## the CSV files
    
    ## pollutant is a character vector of length 1 indicating the name of the
    ## pollutant for which we will calculate the mean; either 'sulfate' or
    ## 'nitrate'
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## Return the mean of the pollutant across all monitors list in the 'id'
    ## vector (ignoring NA values)
    ## Do not round results!
    ## 2. Use a loop to read and source id files indicated in id using the directory path
    # use read.table() to read the filesl
    ## 3. Remove NA before calculating the mean of the pollutant
    
    ## Preparation
    working_dir<-"~/Coursera/r-programming"
    setwd(working_dir)
    fileList<-paste(directory,"/",formatC(id, width=3, flag="0"), ".csv", sep="")
    
    DF<-data.frame()
    for(f in fileList) {
        fData<-read.table(f, sep=",", header = TRUE)
        DF<-rbind(DF, fData)
    }
    
    result<-mean(DF[,pollutant], na.rm=TRUE)
    round(result, digits = 3)
}

complete<-function(directory, id = 1:332) {
    
    # Reads a directory and files, and returns the amount of complete observatios
    # per file
    # Return a data frame with the format:
    # id    nobs
    # 2     1126
    # where id is the name of the file and nobs the number of complete observations
    
    # Step 1: Create empty Data Frame with the name of columns
    
    working_dir<-"~/Coursera/r-programming"
    setwd(working_dir)
    fileList<-paste(directory,"/",formatC(id, width=3, flag="0"), ".csv", sep="")
    DF<-data.frame()
    
    for(f in fileList) {
        fData<-read.table(f, sep=",", header = TRUE)
        row<-as.numeric(c(fData[1,4],sum(complete.cases(fData))))
        DF<-rbind(DF, row)
    }
    colnames(DF) <- c("id","nobs")
    DF
}

corr<-function(directory, threshold = 0) {
    working_dir<-"~/Coursera/r-programming"
    setwd(working_dir)
    fileList<-list.files(directory)
    v<-numeric()
    for(f in fileList) {
        fData<-read.table(paste(directory, f, sep="/"), sep=",", header = TRUE)
        fData<-na.omit(fData)
        # Calculate observations sum(complete.cases(fData))
        if(nrow(fData) >= threshold) {
            v<-append(v, round(cor(fData$sulfate, fData$nitrate), digits = 5))
        }
    }
    v
}