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
terr_daily <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-daily.csv")
  )
terr_30_min <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-30-mins.csv")
)
aquatic_daily <- readr::read_csv(
  here::here("./data/efi-neon-data/aquatic-daily.csv")
)
aquatic_hourly <- readr::read_csv(
  here::here("./data/efi-neon-data/aquatic-hourly.csv")
)
beetles <- readr::read_csv(
  here::here("./data/efi-neon-data/beetles.csv")
)
phenology <- readr::read_csv(
  here::here("./data/efi-neon-data/phenology.csv")
)
ticks <- readr::read_csv(
  here::here("./data/efi-neon-data/ticks.csv")
)

source(here("./R/functions_global.R"))

# data set up ==================================================================

names(terr_daily)
unique(terr_daily$variable)

# currently in long format, so going to pivot to a wide format
terr_day_wide <- tidyr::pivot_wider(
  data = terr_daily, 
  names_from = variable,
  values_from = observation
)

# look at the mean values per datetime ob
avg_terr_day = terr_daily %>% 
  dplyr::group_by(datetime, variable) %>% 
  dplyr::summarize(mean_obs = mean(observation, na.rm = TRUE))


# plot the variable of interest through time
ggplot2::ggplot(data = terr_daily) +
  geom_point(aes(x = datetime, y = observation, fill = variable), 
             shape = 21, alpha = 0.02) + 
  geom_line(data = avg_terr_day, 
            aes(x = datetime, y = mean_obs, colour = variable)) + 
  theme_base()

# let's just do one plot at a time so we can see what's going on
le_day <- terr_daily %>% 
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
nee_day <- terr_daily %>% 
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
str(aquat_daily)
unique(aquat_day$variable)

oxy_day <- aquat_day %>% 
  dplyr::filter(variable == "oxygen")
mean_oxy_day <- oxy_day %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_oxy = mean(observation, na.rm = TRUE))
temp_day <- aquat_day %>% 
  dplyr::filter(variable == "temperature")
mean_temp_day <- temp_day %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_temp = mean(observation, na.rm = TRUE))
chla_day <- aquat_day %>% 
  dplyr::filter(variable == "chla")
mean_chla_day <- chla_day %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_chla = mean(observation, na.rm = TRUE))

ggplot() + 
  geom_point(data = oxy_day, 
             aes(x = datetime, y = observation), 
             shape = 21, alpha = 0.02, fill = "grey80") +
  geom_line(data = mean_oxy_day, 
            aes(x = datetime, y = mean_oxy)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Surface Mean Daily Dissolved Oxygen (mg L<sup>-1</sup>)") +
  theme(
    axis.title.y = element_markdown()
  ) 

ggplot() + 
  geom_point(data = temp_day, 
             aes(x = datetime, y = observation), 
             shape = 21, alpha = 0.02, fill = "orange") +
  geom_line(data = mean_temp_day, 
            aes(x = datetime, y = mean_temp)) + 
  theme_base() + 
  labs(x = "Date", y = "Surface Mean Daily Water Temperature (°C)") +
  theme(
    axis.title.y = element_markdown()
  ) 

ggplot() + 
  geom_point(data = chla_day, 
             aes(x = datetime, y = observation), 
             shape = 21, alpha = 0.02, fill = "green3") +
  geom_line(data = mean_chla_day, 
            aes(x = datetime, y = mean_chla)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Daily Mean Chlorophyll-a Concentration (mg L<sup>-1</sup>)") +
  theme(
    axis.title.y = element_markdown()
  )  + 
  ylim(c(0, 75))

# beetles ======================================================================
unique(beetles$variable)
abund_beetle <- beetles %>% 
  dplyr::filter(variable == "abundance") 
mean_abund_beetle <- abund_beetle %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_abund = mean(observation, na.rm = TRUE))

ggplot(data = abund_beetle) + 
  geom_point(aes(x = datetime, y = observation),
             shape = 21, alpha = 0.1, fill = "purple") + 
  geom_line(data = mean_abund_beetle, aes(x = datetime, y = mean_abund)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Total number of carabids per-trap-night") +
  theme(
    axis.title.y = element_markdown()
  ) +
  ylim(c(0,5))

rich_beetle <- beetles %>% 
  dplyr::filter(variable == "abundance") 
mean_rich_beetle <- abund_beetle %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_abund = mean(observation, na.rm = TRUE))

ggplot(data = rich_beetle) + 
  geom_point(aes(x = datetime, y = observation),
             shape = 21, alpha = 0.1, fill = "maroon") + 
  geom_line(data = mean_abund_beetle, aes(x = datetime, y = mean_abund)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Total number of carabids per-trap-night") +
  theme(
    axis.title.y = element_markdown()
  ) + 
  ylim(c(0,6))


# phenology ====================================================================
unique(phenology$variable)
gcc_90 <- phenology %>% 
  dplyr::filter(variable == "gcc_90") 
mean_gcc_90 <- gcc_90 %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_gcc_90 = mean(observation, na.rm = TRUE))

ggplot(data = gcc_90) + 
  geom_point(aes(x = datetime, y = observation),
             shape = 21, alpha = 0.02, fill = "green") + 
  geom_line(data = mean_gcc_90, aes(x = datetime, y = mean_gcc_90)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Green chromatic coordinate (90th percentile)") +
  theme(
    axis.title.y = element_markdown()
  ) 

rcc_90 <- phenology %>% 
  dplyr::filter(variable == "rcc_90") 
mean_rcc_90 <- rcc_90 %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_rcc_90 = mean(observation, na.rm = TRUE))

ggplot(data = rcc_90) + 
  geom_point(aes(x = datetime, y = observation),
             shape = 21, alpha = 0.02, fill = "red") + 
  geom_line(data = mean_rcc_90, aes(x = datetime, y = mean_rcc_90)) + 
  theme_base() + 
  labs(x = "Date", 
       y = "Red chromatic coordinate (90th percentile)") +
  theme(
    axis.title.y = element_markdown()
  ) 

# ticks ========================================================================
unique(ticks$variable)

mean_ticks <- ticks %>% 
  dplyr::group_by(datetime) %>% 
  dplyr::summarize(mean_ticks = mean(observation, na.rm = TRUE))

ggplot(data = ticks) + 
  geom_point(aes(x = datetime, y = observation),
             shape = 21, alpha = 0.2, fill = "blue4") + 
  geom_line(data = mean_ticks, aes(x = datetime, y = mean_ticks)) + 
  theme_base() + 
  labs(x = "Date", 
       y = expression(
         "Density of *Amblyomma americanum* nympths per week (ticks per 1.6km<sup>2</sup>")) +
  theme(
    axis.title.y = element_markdown()
  ) 


# calculate permutation entropy for the timeseries =============================

terr_daily <- listed_res$terrestrial_daily

str(terr_daily)

le <- terr_daily %>% 
  dplyr::filter(variable == "le")
nee <- terr_daily %>% 
  dplyr::filter(variable == "nee")

# look at each site too
unique(le$site_id)
x <- le$observation
dim <- 3
permu_entropy(x, 3)
all_dims <- c(3:7)
x_s <- list(x)
mapply(permu_entropy, list(x), all_dims)







make_variable_plot <- function(listed_res, challenge, variables, colours,
                               save, output_path = NULL) {
  #' Make a plot for the challenge and variable of interest
  #' 
  #' @description Taking the whole list of the challenge dataframes, specify the 
  #' challenge and the variable at hand, then what colour you want to use 
  #' 
  #' @param listed_res list. A list of all the downloaded NEON data
  #' @param challenge character. Specify which of the challenges should be used.
  #' options are "aquatic_daily", "aquatic_hourly", "terrestrial_30mins", 
  #' "terrestrial_daily", "phenology", "ticks", "beetles"
  #' @param variables character. What variable(s) to be plotted. Any subset of
  #' the variables in a challenge can be used. 
  #' @param colours character. What colour should the points be in the plot
  #' @param save boolean. If the file should be saved or not
  #' @param output_path character. Where to save the files 
  #' 
  #' @usage make_variable_plot(listed_res, "aquatic_daily", "all", 
  #' colours = c("white", "orange", "green"), save = TRUE, 
  #' output_path = here("./figs/neon-data-timeseries/))
  #' @return no returns, but writes out data files
  
  # matching list is to put the y-axes of each of the challenges variables 
  # in one list 
  matching_list <- list(
    aquatic_daily = list(
      oxygen = 
        "Surface mean daily dissolved oxygen concentration (mg L<sup>-1</sup>",
      temperature = "Surface mean daily water temperature (°C)",
      chla = "Mean daily chlorophyll-a concentration (mg L<sup>-1</sup>"
    ),
    aquatic_hourly = list(
      temperature = "Surface hourly water temperature (°C)"
    ),
    terrestrial_30mins = list(
      nee = "Net ecosystem exchange (g C m<sup>-2</sup> day<sup>-1</sup>)",
      le = "Latent Heat Flux (W m<sup>-2</sup>)"
    ),
    terrestrial_daily = list(
      nee = "Net ecosystem exchange (g C m<sup>-2</sup> day<sup>-1</sup>)",
      le = "Latent Heat Flux (W m<sup>-2</sup>)"
    ),
    phenology = list(
      gcc_90 = "Green chromatic coordinate (90th percentile)",
      rcc_90 = "Red chromatic coordinate (90th percentile)"
    ),
    ticks = list(
      amblyomma_americanum = 
        paste0("Density of *Amblyomma americanum* nympths", 
               "per week (ticks per 1.6km<sup>2</sup>")
    ),
    beetles = list(
      abundance = "Total number of carabids per-trap-night",
      ricness = "Total number of unique species in a sampling bout"
    )
  )
  
  if(variables == "all") {
    df <- listed_res$`challenge` 
    for(i in names(matching_list$aquatic_daily)) {
      
      chla_day <- aquat_day %>% 
        dplyr::filter(variable == "chla")
      mean_chla_day <- chla_day %>% 
        dplyr::group_by(datetime) %>% 
        dplyr::summarize(mean_chla = mean(observation, na.rm = TRUE))
      
    }
  }
  
  p <- ggplot2::ggplot(data = listed_res$challenge) 
    
}
