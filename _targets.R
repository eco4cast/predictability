##' Targets file for pipeline management
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all targets to run this analysis. If you are not
#' familiar with the targets package, see the documentation here:
#' https://books.ropensci.org/targets/
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

# set up =======================================================================

library(targets)
library(tarchetypes)
library(here)

# read in function files
source(here::here("./R/functions_global.R"))
source(here::here("./R/functions_neon_data.R"))
source(here::here("./R/functions_permutation_entropy.R"))

# set the packages
targets::tar_option_set(
  packages = c(
    "readr", "here", "entropy", "magrittr", "dplyr", "ggplot2",
    "tidyr", "ggtext", "patchwork"
  ),
  error = "stop"
)

list(
  # data related targets =======================================================
  tar_target(
    download_neon_data,
    pull_data(
      # write controls if copies of the files should be written to the local
      write = FALSE
    )
  )
)
