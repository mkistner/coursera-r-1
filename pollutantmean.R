pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)
  
  ## get the directory in correct format
  ## import the stringr package
  library(stringr)
  ## zero-pad the id's
  padded_ids <- str_pad(id, 3, "0", side="left")
  ## now get full directories
  full_directories <- paste(directory, "/", padded_ids, ".csv", sep="")

  ## get all relevant data
  all_data <- numeric(length(id)) # pre-allocate
  for (i in 1:length(id)) {
    temp <- read.csv(full_directories[i])
    all_data[i] <- temp[pollutant]
  }
  all_data
}
