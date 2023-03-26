##' AUTHOR: Shubhi Sharma
##' DATE OF CREATION: 2023-02-20
#'
#' This file contains functions to calculate permutation entropy from a time
#' series, that are then used to analyze a variety of different data.
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# ordinal_pattern ==============================================================
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

# Function to c
# Input (1 argument, Null argument not valid)
# op = Ordinal pattern computed using the function ordinal_pattern
# op (type=numeric vector)
# Output is normalized permutation entropy (type=numeric)

permu_entropy <- function(op) {
  #' Compute permutation entropy of  a given time series
  #' 
  #' @description This function takes the ordinal pattern of a timeseries as 
  #' returned from ordinal_pattern(), and gives the permutation entropy measure
  #' 
  #' @param op numeric vector. Ordinal pattern vector
  #' 
  #' @usage permu_entropy(op)
  #' @return normalized permutation entropy
  #' @references This function code comes from Srinivasan Radhakrishnan and 
  #' can be found here: https://tinyurl.com/ynmkkfaf
  
  # type checks
  stopifnot(is.numeric(op) & length(op) > 1)
  
  # Compute maximum entropy. maximum entropy = log(dim!)
  # or maximum entropy = log(length(ordinal_pattern))
  entropy_max <- log(length(op))

  # Normalized permutation entropy
  npe <- entropy::entropy(op) / entropy_max
  return(npe)
}
