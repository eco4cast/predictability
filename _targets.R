##' Targets file for pipeline management
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all targets to run this analysis. If you are not
#' familiar with the targets package, see the documentation here:
#' https://books.ropensci.org/targets/
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library. Every function is named using `snake_case`, and every target is
#' named using `camelCase`
#'

# set up =======================================================================

library(targets)
library(tarchetypes)
library(here)

# read in function files
source(here::here("./src/R/functions_global.R"))
source(here::here("./src/R/functions_neon_data.R"))
source(here::here("./src/R/functions_permutation_entropy.R"))
source(here::here("./src/R/functions_autocorr.R"))
source(here::here("./src/R/functions_check_neon_data.R"))

# set the packages
targets::tar_option_set(
  packages = c(
    "readr", "here", "entropy", "magrittr", "dplyr", "ggplot2",
    "tidyr", "ggtext", "patchwork", "ggthemes"
  ),
  error = "stop"
)

list(
  # data related targets =======================================================
  tar_target(
    downloadNeonData,
    pull_data(
      # write controls if copies of the files should be written to the local
      write = TRUE
    )
  ),
  # autocorrelation plots ======================================================
  #' For each data object we're working with, we want a plot of the partial
  #' and full autocorrelation for each location of each data object
  tar_target(ticksAutocorr, autocorr(
    data = get_data_csv(here::here("./data/efi-neon-data/ticks.csv")),
    subfolder = "ticks-by-site",
    datatype = "ticks"
  )),
  tar_target(terrDailyAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/terrestrial-daily.csv")
    ),
    subfolder = "terr-daily-by-site",
    datatype = "terr-daily"
  )),
  tar_target(terr30minsAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/terrestrial-30-mins.csv")
    ),
    subfolder = "terr-30min-by-site",
    datatype = "terr-30min"
  )),
  tar_target(aquaticHourlyAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/aquatic-hourly.csv")
    ),
    subfolder = "aquatic-hourly-by-site",
    datatype = "aquatic-hourly"
  )),
  tar_target(aquaticDailyAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/aquatic-daily.csv")
    ),
    subfolder = "aquatic-daily-by-site",
    datatype = "aquatic-daily"
  )),
  tar_target(beetlesAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/beetles.csv")
    ),
    subfolder = "beetles-by-site",
    datatype = "beetles"
  )),
  tar_target(phenologyAutocorr, autocorr(
    data = get_data_csv(
      here::here("./data/efi-neon-data/phenology.csv")
    ),
    subfolder = "phenology-by-site",
    datatype = "phenology"
  )),
  # prep info for permutation entropy ==========================================
  tar_target(terrDailyPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/terrestrial-daily.csv")
    ),
    challenge = "terr-daily"
  )),
  tar_target(terr30minsPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/terrestrial-30-mins.csv")
    ),
    challenge = "terr-30min"
  )),
  tar_target(aquaticHourlyPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/aquatic-hourly.csv")
    ),
    challenge = "aquatic-hourly"
  )),
  tar_target(aquaticDailyPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/aquatic-daily.csv")
    ),
    challenge = "aquatic-daily"
  )),
  tar_target(beetlesPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/beetles.csv")
    ),
    challenge = "beetles"
  )),
  tar_target(phenologyPECheck, wpe_check_neon_data(
    df = get_data_csv(
      here::here("./data/efi-neon-data/phenology.csv")
    ),
    challenge = "phenology"
  ))
)
