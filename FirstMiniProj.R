#place the data folders in the same directory of this file

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Sets the working directory to the directory of the source file

#Problem 1
pollutantmean <- function(directory, pollutant, id = 1:332){
  files <- list.files(directory)              #assign the file names to files
  column <- c()                               #declare an empty vector
  for(i in id){                               #loop through every id
    file <- na.omit(read.csv(paste(directory, "/", files[i], sep ="")))       #read the current file and remove incomplete cases
    column <- rbind(column, file[pollutant])  #concatenate the specific column of the file to column
  }
  mean <- mean(column[, 1])                   #get the mean value of the column
  return(mean)                                #return mean
}
#Sample code
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)

#Problem 2
complete <- function(directory, id = 1:332){
  files <- list.files(directory)              #assign the file names to files
  result <- data.frame()
  for(i in id){                               #loop through every id
    file <- na.omit(read.csv(paste(directory, "/", files[i], sep ="")))       #read the current file and remove incomplete cases
    result <- rbind(result, c(i, nrow(file))) #concatenate the id and length of the vector with complete cases to result
  }
  names(result) <- c("id", "nobs")            #add column names to the date frame
  return(result)                              #return the data frame
}
#Sample code
#complete("specdata", 1)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)

#Problem 3
corr <- function(directory, threshold = 0){
  files <- list.files(directory)              #assign the file names to files
  result <- c()                               #declare an empty vector called result
  for(i in 1:332){                            #loop through every id
    file <- na.omit(read.csv(paste(directory, "/", files[i], sep ="")))       #read the current file and remove incomplete cases
    sulfate <- file$sulfate                   #assign the column sulfate to sulfate
    nitrate <- file$nitrate                   #assign the column nitrate to nitrate
    if(length(sulfate) > threshold &  length(nitrate) > threshold){ #check if the current file/monitor is greater than the threshold
      result <- c(result, cor(sulfate, nitrate)) #then concatenate the return value of cor into the result
    }
  }
  if(length(result) <= 0){                    #check if no monitors meet the threshold requirement
    return(c())                               #return a vector of length 0
  }
  return(result)                              #return result            
}
#Sample code
#cr <- corr("specdata", 150)
#cr <- corr("specdata", 400)
#cr <- corr("specdata", 5000)
#cr <- corr("specdata")
#head(cr); summary(cr); length(cr)

#Problem 4
showHistogram <- function(directory){
  outcome <- read.csv(paste(directory, "/", "outcome-of-care-measures.csv", sep=""), colClasses = "character")            #read the file and assign it to outcome
  outcome_11 <- as.numeric(outcome[, 11])                                                                                 #assign column 11 to a variable and converts them to numeric
  hist(outcome_11, main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", xlab="Deaths", col="lightblue")    #shows a histogram from the given data
}
#Sample code
#showHistogram("HospData")
