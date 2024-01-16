#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2023-12-06
#' 

#' We need to look at some important characteristics of the timeseries before we
#' actually do the PE calculation: 
#' 
#' 1) number of observations
#' 2) proportion of zeros in the time-series
#' 3) proportion of ties in the time-series
#' 4) gaps
#' 
#' As Pennekamp (2019) states, zero and tie proportions are calculated since 
#' they can pose problems for rank-based PE

# set up =======================================================================

# going to go through the process once iwth one of the datasets, so for now I'll
# just ues terrestrial-daily, for no particular reason
terr_daily <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-daily.csv")
)

# just do this for one site and one variable
terr <- terr_daily[which(terr_daily$site_id == "BLAN" &
                           terr_daily$variable == "le"), ]
terr_n <- nrow(terr)
terr_finite_prop <- sum(is.finite(terr$observation))/terr_n
terr_zeros <- sum(terr$)
                        