##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-04-11
#'
#' This is for testing the calculation of permutation entropy for each of the 
#' NEON EFI challenge datasets
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# set up =======================================================================
library(readr)
library(here)




# going to go through the process once iwth one of the datasets, so for now I'll
# just ues terrestrial-daily, for no particular reason
terr_day <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-daily.csv")
  )
