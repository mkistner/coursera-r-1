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
  id = 1:332
  padded_ids <- str_pad(id, 3, "0", side="left")
  ## now get full directories
  full_directories <- paste(directory, "/", padded_ids, ".csv", sep="")
  
  ## list which id's meet threshold requirement
  comp = complete(directory)
  
  use_id = id[comp["nobs"] > threshold]
  if (length(use_id) > 0) { # if non-empty
    ## get all relevant data
    #sulfates <- numeric(length(use_id)) # pre-allocate
    #nitrates <- numeric(length(use_id))
    result <- numeric(length(use_id))
    for (i in 1:length(use_id)) {
      temp <- read.csv(full_directories[use_id[i]])
      #sulfates[i] <- temp['sulfate']
      #nitrates[i] <- temp['nitrate']
      temp_cor <- cor(temp['sulfate'], temp['nitrate'], use="na.or.complete")
      result[i] <- temp_cor
    }
  }
  else {
    result = numeric()
  }
  result
}