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
source(here::here("./src/R/functions_global.R"))

library(ggplot2)
library(magrittr)

# just do this for one site and one variable
terr <- terr_daily[which(terr_daily$site_id == "BLAN" &
                           terr_daily$variable == "le"), ]
terr_n <- nrow(terr)
terr_finite_prop <- sum(is.finite(terr$observation))/terr_n
terr_zeros <- nrow(terr[which(terr$observation == 0),])
terr_ties_prop <- sum(terr$observation==0)/terr_n
terr_n_gaps <- sum(abs(diff(terr$datetime)) >= 2)         

# for demonstration purposes, make a plot that shows the locations of missing
# observations along with the data 
terr_comp <- data.frame(
  datetime = seq(as.Date(min(terr$datetime)), as.Date(max(terr$datetime)),
                 by = "days")
  ) %>% 
  dplyr::left_join(
    ., 
    y = terr,
    by = "datetime"
  )
  

missing_blan_le <- ggplot() +
  geom_col(data = terr_comp[which(is.na(terr_comp$observation)),],
           aes(x = datetime, y = 200),
           alpha = 0.2, colour = "lightblue") +
  geom_point(data = terr_comp, 
             aes(x = datetime, y = observation)) + 
  labs(x = "time", y = "le") + 
  theme_base()
ggsave(
  here::here("./figs/manually-generated/terr-daily-missing-obs-blan-le.png"),
  missing_blan_le
)
