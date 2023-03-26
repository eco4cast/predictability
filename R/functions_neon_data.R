##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-25
#'
#' This file contains functions that read in and save the data from the EFI
#' NEON Forecasting Challenge (https://projects.ecoforecast.org/neon4cast-docs/)
#' and then does the necessary data cleaning to get them ready to be used in
#' analysis of permutation entropy
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# pull online data from source =================================================
pull_data <- function() {
  #' Get all 5 themes from the website
  #' 
  #' @description Using the targets already prepared by EFI, read in the 
  #' data as data.frame types, and then save them to a local file each 
  #' 
  #' @usage pull_data
  #' @return no returns, but writes out 5 data files
  
  # pull the aquatics challenge ================================================
  aquatic <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-targets.csv.gz")) |> 
    na.omit()
  hourly_aquatic <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-expanded-observations.csv.gz")) |> 
    na.omit()
  
  # pull 
}