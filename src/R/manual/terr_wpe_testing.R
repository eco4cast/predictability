#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-02-12

#' Using this file as a place to try the various ways of grouping the
#' terrestrial daily measurements to see how the various assumptions and
#' options of data grouping work to get some neat results

terr_daily <- readr::read_csv(
    here::here("./data/efi-neon-data/terrestrial-daily.csv")
)

library(ggplot2)
library(magrittr)

source(here::here("./src/R/functions_wpe.R"))

# make a list with all the options of the timeseries to calculate permutation
# entropy for - one for `le` and one for `nee`

nee_terr <- terr_daily[which(terr_daily$variable == "nee"), ]
nee_df_list <- list() # store the results in

# get the different groupings for each site
counter <- 1
for (curr_site in unique(nee_terr$site_id)) {
    temp <- nee_terr[which(nee_terr$site_id == curr_site), ]
    # make dataframe to join the dates to so it's easier to plot
    df_comp <- data.frame(
        datetime = seq(as.Date(min(temp$datetime)), as.Date(max(temp$datetime)),
            by = "days"
        )
    ) %>%
        dplyr::left_join(
            .,
            y = temp,
            by = "datetime"
        ) %>%
        dplyr::mutate(grouping = 1)

    nee_df_list[[counter]] <- df_comp
    # split into a single dataframe for each grouping and add those two
    for (i in c(2, 3, 5, 7)) {
        temp_group <- df_comp # we want just one column in each
        temp_group$grouping <- i
        # make groups to then calculate means in the group_by with
        temp_group[, "grouping_d"] <- rep(1:ceiling(nrow(temp_group) / i),
            each = i
        )[1:nrow(temp_group)]
        # make
    }
}
