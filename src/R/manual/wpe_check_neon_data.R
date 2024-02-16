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
terr_finite_prop <- sum(is.finite(terr$observation)) / terr_n
terr_zeros <- nrow(terr[which(terr$observation == 0), ])
terr_ties_prop <- sum(terr$observation == 0) / terr_n
terr_n_gaps <- sum(abs(diff(terr$datetime)) >= 2)
terr_gaps <- abs(diff(terr$datetime))[which(
  abs(diff(terr$datetime)) >= 2
)]
ggplot() +
  geom_density(aes(x = terr_gaps))

# for demonstration purposes, make a plot that shows the locations of missing
# observations along with the data
terr_comp <- data.frame(
  datetime = seq(as.Date(min(terr$datetime)), as.Date(max(terr$datetime)),
    by = "days"
  )
) %>%
  dplyr::left_join(
    .,
    y = terr,
    by = "datetime"
  )


missing_blan_le <- ggplot() +
  geom_col(
    data = terr_comp[which(is.na(terr_comp$observation)), ],
    aes(x = datetime, y = 200),
    alpha = 0.2, colour = "lightblue"
  ) +
  geom_point(
    data = terr_comp,
    aes(x = datetime, y = observation)
  ) +
  labs(x = "time", y = "le") +
  theme_base()
ggsave(
  here::here("./figs/manually-generated/terr-daily-missing-obs-blan-le.png"),
  missing_blan_le
)

# make the same plots for when we have rolling averages ========================

make_groups <- function(df, groupings) {
  for (i in groupings) {
    col_name <- paste0("grouping_", i, "d")
    df[, col_name] <- rep(1:ceiling(nrow(df) / i), each = i)[1:nrow(df)]
  }

  df
}

terr_fill_options <- make_groups(df = terr_comp, groupings = c(2, 3, 5, 7))

## 2 day option ================================================================
my_mean <- function(data, var1, var2) {
  dplyr::summarise(data, mean({{ var1 }} + {{ var2 }}))
}
my_mean(mtcars, cyl, am)

terr_2d <- terr_fill_options %>%
  dplyr::group_by(grouping_2d) %>%
  dplyr::summarize(
    mean_datetime = mean(datetime),
    mean_obs = mean(observation, na.rm = TRUE)
  )

missing_blan_le_2d <- ggplot() +
  geom_col(
    data = terr_2d[which(is.na(terr_2d$mean_obs)), ],
    aes(x = mean_datetime, y = 200),
    alpha = 0.2, colour = "lightblue"
  ) +
  geom_point(
    data = terr_2d,
    aes(x = mean_datetime, y = mean_obs)
  ) +
  labs(x = "time", y = "le", title = "2-day average") +
  theme_base()
ggsave(
  here::here(paste0(
    "./figs/manually-generated/",
    "terr-daily-missing-obs-blan-le-2d-avg.png"
  )),
  missing_blan_le_2d
)

## 3 day option ================================================================


terr_3d <- terr_fill_options %>%
  dplyr::group_by(grouping_3d) %>%
  dplyr::summarize(
    mean_datetime = mean(datetime),
    mean_obs = mean(observation, na.rm = TRUE)
  )

missing_blan_le_3d <- ggplot() +
  geom_col(
    data = terr_3d[which(is.na(terr_3d$mean_obs)), ],
    aes(x = mean_datetime, y = 200),
    alpha = 0.2, colour = "lightblue"
  ) +
  geom_point(
    data = terr_3d,
    aes(x = mean_datetime, y = mean_obs)
  ) +
  labs(x = "time", y = "le", title = "3-day average") +
  theme_base()
ggsave(
  here::here(paste0(
    "./figs/manually-generated/",
    "terr-daily-missing-obs-blan-le-3d-avg.png"
  )),
  missing_blan_le_3d
)

## 5 day option ================================================================


terr_5d <- terr_fill_options %>%
  dplyr::group_by(grouping_5d) %>%
  dplyr::summarize(
    mean_datetime = mean(datetime),
    mean_obs = mean(observation, na.rm = TRUE)
  )

missing_blan_le_5d <- ggplot() +
  geom_col(
    data = terr_5d[which(is.na(terr_5d$mean_obs)), ],
    aes(x = mean_datetime, y = 200),
    alpha = 0.2, colour = "lightblue"
  ) +
  geom_point(
    data = terr_5d,
    aes(x = mean_datetime, y = mean_obs)
  ) +
  labs(x = "time", y = "le", title = "5-day average") +
  theme_base()
ggsave(
  here::here(paste0(
    "./figs/manually-generated/",
    "terr-daily-missing-obs-blan-le-5d-avg.png"
  )),
  missing_blan_le_5d
)

## 7 day option ================================================================

terr_7d <- terr_fill_options %>%
  dplyr::group_by(grouping_7d) %>%
  dplyr::summarize(
    mean_datetime = mean(datetime),
    mean_obs = mean(observation, na.rm = TRUE)
  )

missing_blan_le_7d <- ggplot() +
  geom_col(
    data = terr_7d[which(is.na(terr_7d$mean_obs)), ],
    aes(x = mean_datetime, y = 200),
    alpha = 0.2, colour = "lightblue"
  ) +
  geom_point(
    data = terr_7d,
    aes(x = mean_datetime, y = mean_obs)
  ) +
  labs(x = "time", y = "le", title = "7-day average") +
  theme_base()
ggsave(
  here::here(paste0(
    "./figs/manually-generated/",
    "terr-daily-missing-obs-blan-le-7d-avg.png"
  )),
  missing_blan_le_7d
)
