complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## get the directory in correct format
  ## import the stringr package
  library(stringr)
  ## zero-pad the id's
  padded_ids <- str_pad(id, 3, "0", side="left")
  ## now get full directories
  full_directories <- paste(directory, "/", padded_ids, ".csv", sep="")
  
  ## calculate how many entries are complete
  n_obs <- numeric(length(id))
  for (i in 1:length(id)) {
    temp <- read.csv(full_directories[i])
    n_obs[i] = sum(!(is.na(temp["sulfate"])) & !(is.na(temp["nitrate"])))
  }
  
  ## store in data frame
  result <- data.frame(id=id, nobs=n_obs)
}