complete <- function(directory, id = 1:332) {
  ## 'directory'  is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  ## where id is the monitor ID number and 'nobs' is the 
  ## number of complete cases
  
  ## Set the working directory
  if(grep("specdata", directory) == 1) {
    wdir <- getwd()
    directory <- paste0(wdir,"/")
    setwd(wdir)
  }
  filenames <- as.character(list.files(directory))
  id.len <- length(id)
  full.data <- rep(0, id.len)
  ## construct CSV file list with the paths
  files <- paste(directory, filenames, sep = "")
  j <- 1
  for (i in id) {
    current_file <- read.csv(files[i],header=T, sep = ",") 
    full.data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  answer <- data.frame(id = id, nobs = full.data)
  print(answer)
  ## tests
  ## complete("specdata", 1)
  ## complete("specdata", 2)
  ## complete("specdata", 1:2)
  ## complete("specdata", c(1,3,5,7,9))
  ## complete("specdata", 3)
}
  