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
library(tidyr)
library(ggplot2)
library(magrittr)
library(ggtext)

# going to go through the process once iwth one of the datasets, so for now I'll
# just ues terrestrial-daily, for no particular reason
terr_day <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-daily.csv")
  )
aquat_day <- readr::read_csv(
  here::here("./data/efi-neon-data/aquatic-daily.csv")
)

# data set up ==================================================================

names(terr_day)
unique(terr_day$variable)

# currently in long format, so going to pivot to a wide format
terr_day_wide <- tidyr::pivot_wider(
  data = terr_day, 
  names_from = variable,
  values_from = observation
)

# look at the mean values per datetime ob
avg_terr_day = terr_day %>% 
  dplyr::group_by(datetime, variable) %>% 
  dplyr::summarize(mean_obs = mean(observation, na.rm = TRUE))


# plot the variable of interest through time
ggplot2::ggplot(data = terr_day) +
  geom_point(aes(x = datetime, y = observation, fill = variable), 
             shape = 21, alpha = 0.02) + 
  geom_line(data = avg_terr_day, 
            aes(x = datetime, y = mean_obs, colour = variable)) + 
  theme_base()

# let's just do one plot at a time so we can see what's going on
le_day <- terr_day %>% 
  dplyr::filter(variable == "le") %>% 
  dplyr::mutate(le = observation) %>% 
  dplyr::select(-variable) 
le_avg_day <- le_day %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_le = mean(le, na.rm = TRUE))

ggplot() + 
  geom_point(data = le_day, 
             aes(x = datetime, y = le), 
             shape = 21, alpha = 0.02, fill = "red") +
  geom_line(data = le_avg_day, 
            aes(x = datetime, y = mean_le)) + 
  theme_base() + 
  labs(x = "Date", y = "Latent Heat Flux (W m^-2)") +
  theme(
    axis.title.y = element_markdown()
  ) + 
  ylim(c(-10, 250))

# now for the nee one
nee_day <- terr_day %>% 
  dplyr::filter(variable == "nee") %>% 
  dplyr::mutate(nee = observation) %>% 
  dplyr::select(-variable) 
nee_avg_day <- nee_day %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_nee = mean(nee, na.rm = TRUE))

ggplot() + 
  geom_point(data = nee_day, 
             aes(x = datetime, y = nee), 
             shape = 21, alpha = 0.02, fill = "blue") +
  geom_line(data = nee_avg_day, 
            aes(x = datetime, y = mean_nee)) + 
  theme_base() + 
  labs(x = "Date", y = "Net ecosystem exchange (g C m^-2 day^-1)") +
  theme(
    axis.title.y = element_markdown()
  ) 

# aquatic daily ================================================================