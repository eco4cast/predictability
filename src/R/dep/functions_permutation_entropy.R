##' AUTHOR: Shubhi Sharma
##' DATE OF CREATION: 2023-02-20
#'
#' This file contains functions to calculate permutation entropy from a time
#' series, that are then used to analyze a variety of different data.
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# calculate ordinal patterns ===================================================
ordinal_pattern <- function(x, dim) {
  #' Compute the ordinal patterns for a given time series
  #' 
  #' @description This function takes in a time series and gets the ordinal 
  #' patterns which are needed to actually calculate the permuatation entropy. 
  #' Note here that we are using a permuatation entropy calculation that is 
  #' described by Bandt and Pompe (2002 - Phy R Lett)
  #' 
  #' @param x numeric vector. The time series
  #' @param dim numeric. Embedding dimension used for the actual calculation.
  #' Typical values are 3-7.
  #' 
  #' @usage ordinal_pattern(x, dim)
  #' @return numeric vector of size dim
  #' @references This function code comes from Srinivasan Radhakrishnan and 
  #' can be found here: https://tinyurl.com/ynmkkfaf
  
  # type tests
  stopifnot(is.numeric(x) & length(x) > 1)
  stopifnot(is.numeric(dim) & length(dim) == 1)
  
  # Generate ordinal numbers to assign. For example if dim =3, then
  # ordinal number=0,1,2
  ordinal_numbers <- seq(0, (dim - 1), by = 1)

  # Compute all possible permutations of the ordinal numbers.
  # Maximum size of possible_pattern=dim!
  possible_pattern <- (combinat::permn(ordinal_numbers))

  # Initialize result. Result is the output.
  result <- 0
  result[seq_len(length(possible_pattern))] <- 0

  # Loop for computation of ordinal pattern
  for (i in 1:(length(x) - (dim - 1))) {
    temp <- x[i:(i + (dim - 1))]
    tempseq <- seq(0, dim - 1, by = 1)
    tempdata <- data.frame(temp, tempseq)
    tempdata <- tempdata[order(temp), ]

    for (j in seq_len(length(possible_pattern))) {
      if (all(possible_pattern[[j]] == tempdata$tempseq)) {
        result[j] <- result[j] + 1
      }
    }
  }
  
  return(result)
}

# calculate permuatation entropy ===============================================
permu_entropy <- function(x, dim) {
  #' Compute permutation entropy of  a given time series
  #' 
  #' @description This function takes the ordinal pattern of a timeseries as 
  #' returned from ordinal_pattern(), and gives the permutation entropy measure
  #' 
  #' @param x numeric vector. The time series
  #' @param dim numeric. Embedding dimension used for the actual calculation.
  #' Typical values are 3-7.
  #' 
  #' @usage permu_entropy(x, dim)
  #' @return normalized permutation entropy
  #' @references This function code comes from Srinivasan Radhakrishnan and 
  #' can be found here: https://tinyurl.com/ynmkkfaf
  
  # calculate ordinal pattern
  op <- ordinal_pattern(x, dim)
  
  # type checks
  stopifnot(is.numeric(op) & length(op) > 1)
  
  # Compute maximum entropy. maximum entropy = log(dim!)
  # or maximum entropy = log(length(ordinal_pattern))
  entropy_max <- log(length(op))

  # Normalized permutation entropy
  npe <- entropy::entropy(op) / entropy_max
  return(npe)
}

# get both things from the timeseries ==========================================
perm_ent_calc <- function(x, dim = NULL, dim_all = FALSE) {
  #' Data wrapper function
  #' 
  #' @description A simple wrapper for the two other permutation related 
  #' functions that gets the data ready and returns the npe 
  #' 
  #' @param x numeric vector. The time series
  #' @param dim numeric. Embedding dimension used for the actual calculation.
  #' Typical values are 3-7. If using this parameter, pass a single numeric 
  #' value
  #' @param dim_all boolean. Use all possible embedding dimensions, 3-7? This 
  #' is for the case of not having a specific embedding dimension you want to 
  #', use, so the estimate will be generated for each. Default is FALSE
  #' 
  #' @usage perm_ent_calc(x, dim_all = TRUE)
  #' @return a numeric vector of length 1 or 5
  
  # check if all embedding dimensions should be used 
  if(dim_all == FALSE) {
    
    # make sure dim is passed
    if(!is.numeric(dim)) stop(paste0("ERROR - dim not found, you must pass an ",
                                     "embedding dimension if dim_all = FALSE"))
    
    # calculate the op 
    npe <- permu_entropy(x, dim)
    
    return(npe)
  }
  
  # if all the embedding dimensions are to be used, then do that this way 
  npe_vec <- vector(mode = "numeric", length = 5)
  all_dims <- c(3:7)
  
  # use an mapply (recycles x here) to get the values
  npe_vec <- mapply(permu_entropy, list(x), all_dims)
  
  return(npe_vec)
  
}
