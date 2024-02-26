#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-02-12

#' Using this file as a place to try the various ways of grouping the
#' terrestrial daily measurements to see how the various assumptions and
#' options of data grouping work to get some neat results

# terrestrial daily version of this ============================================

terr_daily <- readr::read_csv(
    here::here("./data/efi-neon-data/terrestrial-daily.csv")
)

library(ggplot2)
library(magrittr)

source(here::here("./src/R/functions_wpe.R"))
source(here::here("./src/R/functions_global.R"))

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
    counter <- counter + 1
    # split into a single dataframe for each grouping and add those two
    for (i in c(2, 3, 5, 7)) {
        temp_group <- df_comp # we want just one column in each
        temp_group$grouping <- i
        # make groups to then calculate means in the group_by with
        temp_group[, "grouping_d"] <- rep(1:ceiling(nrow(temp_group) / i),
            each = i
        )[1:nrow(temp_group)]
        # make the values from summarize
        temp_summarized <- temp_group %>%
            dplyr::group_by(grouping_d) %>%
            dplyr::summarize(
                datetime = mean(datetime),
                observation = mean(observation, na.rm = TRUE)
            )
        # to assign the mean values to each of the dates, let's re-join the
        # summarized dataframe to the other one
        x_df <- temp_group[, c(
            "datetime", "site_id", "variable", "grouping", "grouping_d"
        )]
        # the expanded dates have NA's so we'll fix that here
        x_df$site_id <- curr_site
        x_df$variable <- "nee"
        x_df$grouping_d <- as.factor(x_df$grouping_d)

        y_df <- temp_summarized[, c("grouping_d", "observation")]
        y_df$grouping_d <- as.factor(y_df$grouping_d)

        prepped_df <- dplyr::left_join(
            x = x_df,
            y = y_df,
            by = "grouping_d"
        ) %>%
            dplyr::select(datetime, site_id, variable, observation, grouping)

        nee_df_list[[counter]] <- prepped_df
        counter <- counter + 1
    }
}

le_terr <- terr_daily[which(terr_daily$variable == "le"), ]
le_df_list <- list() # store the results in

# get the different groupings for each site
counter <- 1
for (curr_site in unique(le_terr$site_id)) {
    temp <- le_terr[which(le_terr$site_id == curr_site), ]
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

    le_df_list[[counter]] <- df_comp
    counter <- counter + 1
    # split into a single dataframe for each grouping and add those two
    for (i in c(2, 3, 5, 7)) {
        temp_group <- df_comp # we want just one column in each
        temp_group$grouping <- i
        # make groups to then calculate means in the group_by with
        temp_group[, "grouping_d"] <- rep(1:ceiling(nrow(temp_group) / i),
            each = i
        )[1:nrow(temp_group)]
        # make the values from summarize
        temp_summarized <- temp_group %>%
            dplyr::group_by(grouping_d) %>%
            dplyr::summarize(
                datetime = mean(datetime),
                observation = mean(observation, na.rm = TRUE)
            )
        # to assign the mean values to each of the dates, let's re-join the
        # summarized dataframe to the other one
        x_df <- temp_group[, c(
            "datetime", "site_id", "variable", "grouping", "grouping_d"
        )]
        # the expanded dates have NA's so we'll fix that here
        x_df$site_id <- curr_site
        x_df$variable <- "le"
        x_df$grouping_d <- as.factor(x_df$grouping_d)

        y_df <- temp_summarized[, c("grouping_d", "observation")]
        y_df$grouping_d <- as.factor(y_df$grouping_d)

        prepped_df <- dplyr::left_join(
            x = x_df,
            y = y_df,
            by = "grouping_d"
        ) %>%
            dplyr::select(datetime, site_id, variable, observation, grouping)

        le_df_list[[counter]] <- prepped_df
        counter <- counter + 1
    }
}

## run through the PE calculations ==============================================

le_PE_df <- data.frame(
    site_id = as.character(),
    wpe = as.numeric(),
    grouping = as.numeric()
)
nee_PE_df <- data.frame(
    site_id = as.character(),
    wpe = as.numeric(),
    grouping = as.numeric()
)

for (i in seq_len(length(le_df_list))) {
    # get the site_id
    le_PE_df[i, "site_id"] <- unique(le_df_list[[i]]$site_id)[
        which(!is.na(unique(le_df_list[[i]]$site_id)))
    ]
    # the grouping
    le_PE_df[i, "grouping"] <- unique(le_df_list[[i]]$grouping)[
        which(!is.na(unique(le_df_list[[i]]$grouping)))
    ]
    # now the PE
    le_PE_df[i, "wpe"] <- tryCatch(
        PE(le_df_list[[i]]$observation,
            weighted = T, tau = 1, word_length = 3, tie_method = "first"
        ),
        error = function(err) NA
    )
    # for the other variable
    nee_PE_df[i, "site_id"] <- unique(nee_df_list[[i]]$site_id)[
        which(!is.na(unique(nee_df_list[[i]]$site_id)))
    ]
    nee_PE_df[i, "grouping"] <- unique(nee_df_list[[i]]$grouping)[
        which(!is.na(unique(nee_df_list[[i]]$grouping)))
    ]
    nee_PE_df[i, "wpe"] <- tryCatch(
        PE(nee_df_list[[i]]$observation,
            weighted = T, tau = 1, word_length = 3, tie_method = "first"
        ),
        error = function(err) NA
    )
}

le_PE_df$grouping <- as.factor(le_PE_df$grouping)
nee_PE_df$grouping <- as.factor(nee_PE_df$grouping)

p_le <- ggplot(data = le_PE_df) +
    geom_violin(aes(
        x = grouping, y = wpe, group = grouping,
        fill = grouping
    ), colour = "black") +
    theme_base() +
    labs(
        x = "Rolling Window Avg", y = "Weighted Permutation Energy"
    )
ggsave(
    here::here(paste0(
        "./figs/manually-generated/",
        "terr-daily-le-wpe-rolling-window.png"
    )),
    p_le
)
p_nee <- ggplot(data = nee_PE_df) +
    geom_violin(aes(
        x = grouping, y = wpe, group = grouping,
        fill = grouping
    ), colour = "black") +
    theme_base() +
    labs(
        x = "Rolling Window Avg", y = "Weighted Permutation Energy"
    )
ggsave(
    here::here(paste0(
        "./figs/manually-generated/",
        "terr-daily-nee-wpe-rolling-window.png"
    )),
    p_nee
)
phenologyPECheck <- tar_read(phenologyPECheck)


# beetles versiion =============================================================

beetles <- readr::read_csv(
    here::here("./data/efi-neon-data/beetles.csv")
)

abund_beetles <- beetles[which(beetles$variable == "abundance"), ]
abund_list <- list() # store the results in

# get the different groupings for each site
counter <- 1
for (curr_site in unique(abund_beetles$site_id)) {
    temp <- abund_beetles[which(abund_beetles$site_id == curr_site), ]
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

    abund_list[[counter]] <- df_comp
    counter <- counter + 1
    # split into a single dataframe for each grouping and add those two
    for (i in c(2, 3, 5, 7)) {
        temp_group <- df_comp # we want just one column in each
        temp_group$grouping <- i
        # make groups to then calculate means in the group_by with
        temp_group[, "grouping_d"] <- rep(1:ceiling(nrow(temp_group) / i),
            each = i
        )[1:nrow(temp_group)]
        # make the values from summarize
        temp_summarized <- temp_group %>%
            dplyr::group_by(grouping_d) %>%
            dplyr::summarize(
                datetime = mean(datetime),
                observation = mean(observation, na.rm = TRUE)
            )
        # to assign the mean values to each of the dates, let's re-join the
        # summarized dataframe to the other one
        x_df <- temp_group[, c(
            "datetime", "site_id", "variable", "grouping", "grouping_d"
        )]
        # the expanded dates have NA's so we'll fix that here
        x_df$site_id <- curr_site
        x_df$variable <- "abundance"
        x_df$grouping_d <- as.factor(x_df$grouping_d)

        y_df <- temp_summarized[, c("grouping_d", "observation")]
        y_df$grouping_d <- as.factor(y_df$grouping_d)

        prepped_df <- dplyr::left_join(
            x = x_df,
            y = y_df,
            by = "grouping_d"
        ) %>%
            dplyr::select(datetime, site_id, variable, observation, grouping)

        abund_list[[counter]] <- prepped_df
        counter <- counter + 1
    }
}

rich_beetles <- beetles[which(beetles$variable == "richness"), ]
rich_list <- list() # store the results in

# get the different groupings for each site
counter <- 1
for (curr_site in unique(rich_beetles$site_id)) {
    temp <- rich_beetles[which(rich_beetles$site_id == curr_site), ]
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

    rich_list[[counter]] <- df_comp
    counter <- counter + 1
    # split into a single dataframe for each grouping and add those two
    for (i in c(2, 3, 5, 7)) {
        temp_group <- df_comp # we want just one column in each
        temp_group$grouping <- i
        # make groups to then calculate means in the group_by with
        temp_group[, "grouping_d"] <- rep(1:ceiling(nrow(temp_group) / i),
            each = i
        )[1:nrow(temp_group)]
        # make the values from summarize
        temp_summarized <- temp_group %>%
            dplyr::group_by(grouping_d) %>%
            dplyr::summarize(
                datetime = mean(datetime),
                observation = mean(observation, na.rm = TRUE)
            )
        # to assign the mean values to each of the dates, let's re-join the
        # summarized dataframe to the other one
        x_df <- temp_group[, c(
            "datetime", "site_id", "variable", "grouping", "grouping_d"
        )]
        # the expanded dates have NA's so we'll fix that here
        x_df$site_id <- curr_site
        x_df$variable <- "richness"
        x_df$grouping_d <- as.factor(x_df$grouping_d)

        y_df <- temp_summarized[, c("grouping_d", "observation")]
        y_df$grouping_d <- as.factor(y_df$grouping_d)

        prepped_df <- dplyr::left_join(
            x = x_df,
            y = y_df,
            by = "grouping_d"
        ) %>%
            dplyr::select(datetime, site_id, variable, observation, grouping)

        rich_list[[counter]] <- prepped_df
        counter <- counter + 1
    }
}


## run through the PE calculations ==============================================

abund_PE_df <- data.frame(
    site_id = as.character(),
    wpe = as.numeric(),
    grouping = as.numeric()
)
rich_PE_df <- data.frame(
    site_id = as.character(),
    wpe = as.numeric(),
    grouping = as.numeric()
)

for (i in seq_len(length(abund_list))) {
    # get the site_id
    abund_PE_df[i, "site_id"] <- unique(abund_list[[i]]$site_id)[
        which(!is.na(unique(abund_list[[i]]$site_id)))
    ]
    # the grouping
    abund_PE_df[i, "grouping"] <- unique(abund_list[[i]]$grouping)[
        which(!is.na(unique(abund_list[[i]]$grouping)))
    ]
    # now the PE
    abund_PE_df[i, "wpe"] <- tryCatch(
        PE(abund_list[[i]]$observation,
            weighted = T, tau = 1, word_length = 3, tie_method = "first"
        ),
        error = function(err) NA
    )
    # for the other variable
    rich_PE_df[i, "site_id"] <- unique(rich_list[[i]]$site_id)[
        which(!is.na(unique(nee_df_list[[i]]$site_id)))
    ]
    rich_PE_df[i, "grouping"] <- unique(rich_list[[i]]$grouping)[
        which(!is.na(unique(rich_list[[i]]$grouping)))
    ]
    rich_PE_df[i, "wpe"] <- tryCatch(
        PE(rich_list[[i]]$observation,
            weighted = T, tau = 1, word_length = 3, tie_method = "first"
        ),
        error = function(err) NA
    )
}

abund_PE_df$grouping <- as.factor(abund_PE_df$grouping)
rich_PE_df$grouping <- as.factor(rich_PE_df$grouping)

p_abund <- ggplot(data = abund_PE_df) +
    geom_violin(aes(
        x = grouping, y = wpe, group = grouping,
        fill = grouping
    ), colour = "black") +
    theme_base() +
    labs(
        x = "Rolling Window Avg", y = "Weighted Permutation Energy"
    )
ggsave(
    here::here(paste0(
        "./figs/manually-generated/",
        "terr-daily-le-wpe-rolling-window.png"
    )),
    p_le
)
p_nee <- ggplot(data = nee_PE_df) +
    geom_violin(aes(
        x = grouping, y = wpe, group = grouping,
        fill = grouping
    ), colour = "black") +
    theme_base() +
    labs(
        x = "Rolling Window Avg", y = "Weighted Permutation Energy"
    )
ggsave(
    here::here(paste0(
        "./figs/manually-generated/",
        "terr-daily-nee-wpe-rolling-window.png"
    )),
    p_nee
)
