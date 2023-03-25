# Permutation Entropy is computed using the method prescribed by Bandt and Pompe
# Bandt, C., & Pompe, B. (2002). Permutation entropy: a natural complexity measure for time series. Physical review letters, 88(17), 174102. #nolint
# Code from https://github.com/srk-srinivasan/Permutation-Entropy/blob/master/permu_entropy.R #nolint

# Function to compute the ordinal patterns for a given time series.
# Input (2 arguments. Null arguments are not vaild)
# x = Given time series (type=numeric vector)
# dim = Embedding dimension (type=numeric)
# Commonly used value of dim ranges from 3 to 7
# Output is a numeric vector of size=(dim)!

ordinal_pattern <- function(x, dim) {
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

# Function to compute permutation entropy of  a given time series
# Input (1 argument, Null argument not valid)
# op = Ordinal pattern computed using the function ordinal_pattern
# op (type=numeric vector)
# Output is normalized permutation entropy (type=numeric)

permu_entropy <- function(op) {
  # Compute maximum entropy. maximum entropy = log(dim!)
  # or maximum entropy = log(length(ordinal_pattern))
  entropy_max <- log(length(op))

  # Normalized permutation entropy
  npe <- entropy::entropy(op) / entropy_max
  return(npe)
}
