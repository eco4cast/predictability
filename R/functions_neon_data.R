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
pull_data <- function(write = FALSE) {
  #' Get all 5 themes from the website
  #' 
  #' @description Using the targets already prepared by EFI, read in the 
  #' data as data.frame types, and then save them to a local file each 
  #' 
  #' @usage pull_data
  #' @return no returns, but writes out 5 data files
  
  ## pull the aquatics challenge ===============================================
  aquatic <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-targets.csv.gz")) |> 
    na.omit()
  hourly_aquatic <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-expanded-observations.csv.gz")) |> 
    na.omit()
  
  ## pull terrestrial data =====================================================
  terr_30_min <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "terrestrial_30min/terrestrial_30min-targets.csv.gz"), 
          guess_max = 1e6)
  terr_daily <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "terrestrial_daily/terrestrial_daily-targets.csv.gz"), 
          guess_max = 1e6) |> 
    na.omit()
  
  ## pull ticks data ===========================================================
  ticks <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "ticks/ticks-targets.csv.gz"),
    guess_max = 1e6)
  
  ## pull phenology data =======================================================
  phenology <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "phenology/phenology-targets.csv.gz"),
    guess_max = 1e6) |> 
    na.omit()
  
  ## pull beetles data =========================================================
  beetles <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "beetles/beetles-targets.csv.gz"),
    guess_max = 1e6)

  # if write is TRUE, then write the data to files 
  if(write == TRUE) {
    readr::write_csv(
      x = aquatic,
      file = here::here("./data/efi-neon-data/aquatic-daily.csv")
    )
    readr::write_csv(
      x = hourly_aquatic,
      file = here::here("./data/efi-neon-data/aquatic-hourly.csv")
    )
    readr::write_csv(
      x = terr_daily,
      file = here::here("./data/efi-neon-data/terrestrial-daily.csv")
    )
    readr::write_csv(
      x = terr_30_min,
      file = here::here("./data/efi-neon-data/terrestrial-30-mins.csv")
    )
    readr::write_csv(
      x = ticks,
      file = here::here("./data/efi-neon-data/ticks.csv")
    )
    readr::write_csv(
      x = phenology,
      file = here::here("./data/efi-neon-data/phenology.csv")
    )
    readr::write_csv(
      x = beetles,
      file = here::here("./data/efi-neon-data/beetles.csv")
    )
  }
  
  listed_res <- list(aquatic, hourly_aquatic, terr_30_min, terr_daily, 
                     phenology, ticks, beetles)
  names(listed_res) <- c("aquatic_daily", "aquatic_hourly", 
                         "terrestrial_30mins", "terrestrial_daily", 
                         "phenology", "ticks", "beetles")
  
  return(listed_res)
}