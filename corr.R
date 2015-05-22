corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Returns a numeric vector of correlations
  
  ## get the directory in correct format
  ## import the stringr package
  library(stringr)
  ## zero-pad the id's
  padded_ids <- str_pad(id, 3, "0", side="left")
  ## now get full directories
  full_directories <- paste(directory, "/", padded_ids, ".csv", sep="")
  
  ## list which id's meet threshold requirement
  comp = complete(directory)
  id = 1:332
  use_id = id[comp >= threshold]
  
  ## get all relevant data
  sulfates <- numeric(length(use_id)) # pre-allocate
  nitrates <- numeric(length(use_id))
  for (i in 1:length(id)) {
    temp <- read.csv(full_directories[i])
    sulfates[i] <- temp[,'sulfate']
    nitrates[i] <- temp[,'nitrate']
  }
  all_sulfates <- unlist(sulfates)
  all_nitrates <- unlist(nitrates)
  
  result = cor(all_sulfates, all_nitrates, use="complete.obs")
}