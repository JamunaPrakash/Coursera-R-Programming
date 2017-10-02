corr <- function(directory, threshold=0) {
  ## 'directory'  is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of 
  ## complete observations (on all variables) required to compute the
  ## correlation between nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## Set the working directory
  if(grep("specdata", directory) == 1) {
    wdir <- getwd()
    directory <- paste0(wdir,"/")
    setwd(wdir)
  }
  specdata.table <- complete("specdata", 1:332)
  nobs <- specdata.table$nobs
  ids <- specdata.table$id[nobs > threshold]
  ids.len <- length(ids)
  corr.data <- rep(0, ids.len)
  filenames <- as.character(list.files(directory))
  ## construct CSV file list with the paths
  files <- paste(directory, filenames, sep = "")
  j <- 1
  for (i in ids) {
    current_file <- read.csv(files[i],header=T, sep = ",") 
    corr.data[j] <- cor(current_file$sulfate, current_file$nitrate, use = "complete.obs")
    j <- j + 1
  }
  answer <- corr.data
  return(answer)
  ## tests
  ## testcor <- corr("specdata", 150)
  ## head(testcor)
  ## testcor <- corr("specdata", 200)
  ## length(testcor)
  ## testcor <- corr("specdata", 2000)
  ## summary(testcor)
}