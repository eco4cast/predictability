##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-04-11
#'
#' This file contains functions to calculate the permutation entropy for each of
#' the datasets in the EFI Neon Forecasting challenge
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

aquatics_perm_ent <- function(listed_res, data_path, fig_path) {
  #' Calculate permutation entropy for the phenology challenge
  #' 
  #' @description Do the various data chopping that should be done to get the 
  #' aquatics 
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
}
