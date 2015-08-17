setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

#require dplyr package
library(dplyr)

# cached data set for summarySCC_PM25.rds
cacheDF <- makeCacheDataFrame("summarySCC_PM25.rds")

## This is a function to mak and store within cache
makeCacheDataFrame <- function(x = "summarySCC_PM25.rds") {
  ## Initialise data frame
  i <- NULL
  
  ## Function to set file name data frame 
  setFileName <- function(y){
    x <<- y
    i <<- NULL 
  }
  
  ## Get FileName
  getFileName <- function(){
    ## Simply returns matrix
    x
  }
  
  ## Set Data Frame to cache
  setData <- function(df){
    i <<- df
  }
  
  ## Retrieve Data Frame
  getData <- function(){
    i
  }
  
  ## list of subfunctions present
  list(setFileName=setFileName, getFileName=getFileName, setData=setData, getData=getData)
}


## This returns data frame from Rds FileName
## or by pulling from cache if present
cacheData <- function(x, ...) {
  ## Return a data frame 
  m <- x$getData()
  
  ## if data frame already exists in cache, retrieve it
  if( !is.null(m)){
    message("Retrieving Data Frame from cache")
    return(m)
  }
  
  ## Get RDS Filename
  rdsFile <- x$getFileName()
  
  ## get data frame by readRDS
  m <- readRDS(rdsFile,...)
  
  ## Set Data frame to cache
  x$setData(m)
  
  ## Return data frame
  m
}