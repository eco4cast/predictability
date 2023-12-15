library(magrittr)
library(ggplot2)

source(here::here("./src/R/functions_global.R"))

# ticks ========================================================================

ticks <- readr::read_csv(here::here("./data/efi-neon-data/ticks.csv"))

site = "BLAN"
ticks_site <- ticks[which(ticks$site_id == site), ]
ticks_site_plot <- ggplot2::ggplot(data = ticks_site) + 
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
  ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                      size = 2, fill = "red3", shape = 21) +
  theme_base() + 
  labs(y = "# of Ticks", x = "Time")
ggsave(
  paste0(here::here("./figs/neon-data-timeseries/ticks-by-site/"),
         "ticks-", site, ".png"),
  ticks_site_plot
)

obs <- ticks_site$observation

png(
  paste0(here::here("./figs/neon-autocorrelation-plots/"),
         "ticks-", site, ".png"),
)
acf(obs,pl=TRUE)

png(
  paste0(here::here("./figs/neon-autocorrelation-plots/"),
         "partial-ticks-", site, ".png"),
)
pacf(obs,pl=TRUE)
dev.off()


# terrestrial ==================================================================
terr_day <- readr::read_csv(
  here::here("./data/efi-neon-data/terrestrial-daily.csv"))
unique(terr_day$site_id)
site <- "BART"

terr_day_site <- terr_day[which(terr_day$site_id == site), ]

ggplot() + 
  geom_line(data = terr_day_site,
                     aes(x = datetime, y = observation, colour = variable)) + 
  geom_point(data = terr_day_site,
                      aes(x = datetime, y = observation, colour = variable),
                      size = 2, alpha = 0.2) +
  theme_base() + 
  labs(y = "Latent heat flux / Net Ecosystem Exchange", x = "Time") 

obs_le <- terr_day_site$observation[which(terr_day_site$variable == "le")]
obs_nee <- terr_day_site$observation[which(terr_day_site$variable == "nee")]

png(
  paste0(here::here("./figs/neon-autocorrelation-plots/"),
         "ticks-", site, ".png"),
)
acf(obs,pl=TRUE)

png(
  paste0(here::here("./figs/neon-autocorrelation-plots/"),
         "partial-ticks-", site, ".png"),
)
pacf(obs,pl=TRUE)
dev.off()


