pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory'  is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'polutant' is a character vector of length 1 indicating the
  ## name of the pollutant for which we will calculate the 
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ## Set the working directory
  if(grep("specdata", directory) == 1) {
    wdir <- getwd()
    directory <- paste0(wdir,"/")
    setwd(wdir)
  }
  filenames <- as.character(list.files(directory))
  poll.mean = c()
  ## construct CSV file list with the paths
  files <- paste(directory, filenames, sep = "")
  for (i in id) {
    current_file <- read.csv(files[i],header=T, sep = ",") 
    na_excluded <- current_file[!is.na(current_file[, pollutant]), pollutant]
    poll.mean <- c(poll.mean, na_excluded)
  }
  answer <- mean(poll.mean)
  return(answer)
  ## tests
  ## pollutantmean("specdata", "sulfate", 1:10)
  ## pollutantmean("specdata", "nitrate", 70:72)
  ## pollutantmean("specdata", "nitrate", 23)
}