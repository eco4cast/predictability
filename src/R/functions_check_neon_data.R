##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2024-02-15
#'
#' This file contains functions to calculate get the important information
#' we need to know before we pass things through the PE calculation
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

#' @title dyn_grouping
#'
#' @description This function allows you to do easy dynamic grouping, which is
#' supposed to be supported in dplyr but I can't get to work
#'
#' @param df A dataframe of the NEON data
#' @param var_name The variable to group by
#'
#' @return df with all the groupings applied
#' @export
#' @examples
#' dyn_grouping(df, "grouping_2d")
dyn_grouping <- function(df, var_name) {
    dplyr::group_by(df, {{ var_name }})
}

#' @title make_groups
#'
#' @description This function takes the dataframe and calculates rolling
#' averages for each scale of aggregation
#'
#' @param df A dataframe of the NEON data
#' @param challenge The name of the challenge information
#'
#' @return df with all the gap options
#' @export
#' @examples
#' make_groups(df, groupings = 2)
make_groups <- function(df, groupings = c(2, 3, 5, 7)) {
    for (i in groupings) {
        col_name <- paste0("grouping_", i, "d")
        df[, col_name] <- rep(1:ceiling(nrow(df) / i), each = i)[1:nrow(df)]
    }
    return(df)
}

#' @title plot_gaps
#'
#' @description This function plots the gaps in the time-series for each
#' site and variable for a given challenge dataset. Writes files of each plot
#'
#' @param df A dataframe of the NEON data
#' @param challenge The name of the challenge information
#' @param site_id The site ID of the data
#' @param variable The variable of the data
#'
#' @return None
#' @export
#' @examples
#' plot_gaps(df)
plot_gaps <- function(df, challenge, site_id, variable) {
    # make dataframe to join the dates to so it's easier to plot
    df_comp <- data.frame(
        datetime = seq(as.Date(min(df$datetime)), as.Date(max(df$datetime)),
            by = "days"
        )
    ) %>%
        dplyr::left_join(
            .,
            y = df,
            by = "datetime"
        )
    # plot the gaps in the raw time-series
    raw_missing <- ggplot() +
        geom_col(
            data = df_comp[which(is.na(df_comp$observation)), ],
            aes(x = datetime, y = (max(
                df_comp$observation,
                na.rm = TRUE
            ) * 1.1)),
            alpha = 0.2, width = 0.001, colour = "lightblue"
        ) +
        geom_point(data = df_comp, aes(x = datetime, y = observation)) +
        labs(x = "time", y = variable, paste0(
            "Gaps in raw time-series for ", site_id, " ", variable
        )) +
        theme_base()
    # save the plot
    ggsave(
        here::here(
            paste0(
                "./figs/neon-data-gaps/", challenge, "/",
                site_id, "-", variable, "-", "raw-timeseries", ".png"
            )
        ),
        raw_missing
    )
    # now plot the different rolling averages
    for (rolling in c(2, 3, 5, 7)) {
        # make the rolling averages
        df_fill_options <- make_groups(df = df_comp, groupings = rolling)
        df_roll <- df_fill_options %>%
            dplyr::group_by(.data[[paste0("grouping_", rolling, "d")]]) %>%
            dplyr::summarize(
                mean_datetime = mean(datetime),
                mean_obs = mean(observation, na.rm = TRUE)
            )
        # plot the gaps in the rolling averages
        roll_missing <- ggplot() +
            geom_col(
                data = df_roll[which(is.na(df_roll$mean_obs)), ],
                aes(x = mean_datetime, y = (max(df_roll$mean_obs, na.rm = TRUE) * 1.1)),
                alpha = 0.2, colour = "lightblue", width = 0.001
            ) +
            geom_point(data = df_roll, aes(x = mean_datetime, y = mean_obs)) +
            labs(x = "time", y = variable, title = paste0(
                "Gaps with rolling average of ", rolling, " days"
            )) +
            theme_base()
        # save the plot
        ggsave(
            here::here(
                paste0(
                    "./figs/neon-data-gaps/", challenge, "/", site_id, "-",
                    variable, "-", "rolling-average-", rolling, "-days.png"
                )
            ),
            roll_missing
        )
    }
}

#' @title wpe_check_neon_data
#'
#' @description This function checks the data for the following:
#' 1) number of observations
#' 2) proportion of zeros in the time-series
#' 3) proportion of ties in the time-series
#' 4) gaps
#' This also writes a formatted table to file with the results and plots the
#' gaps in the time-series for each site/variable combo
#'
#' @param df A dataframe of the NEON data
#' @param challenge The name of the challenge information
#'
#' @return A dataframe of the info for that given challenge
#' @export
#' @examples
#' wpe_check_neon_data(df)
wpe_check_neon_data <- function(df, challenge) {
    # loop through each site ID and calculate the necessary info
    df_info <- df %>%
        dplyr::select(site_id, variable) %>%
        unique()

    # empty df to fill
    results <- data.frame(
        site_id = as.character(),
        variable = as.character(),
        n = as.numeric(),
        finite_prop = as.numeric(),
        zeros = as.numeric(),
        ties_prop = as.numeric(),
        n_gaps = as.numeric()
    )
    # go through each site and variable
    for (row in seq_len(nrow(df_info))) {
        # get subset dataframe for this site and variable
        temp <- df[which(df$site_id == df_info$site_id[row] &
            df$variable == df_info$variable[row]), ]

        # make plot
        plot_gaps(
            df = temp, challenge = challenge,
            site_id = df_info[row, "site_id"],
            variable = df_info[row, "variable"]
        )
        # calculate the necessary info
        results_temp <- data.frame(
            site_id = temp[1, "site_id"],
            variable = temp[1, "variable"],
            n = nrow(temp),
            finite_prop = sum(is.finite(temp$observation)) / nrow(temp),
            zeros = nrow(temp[which(temp$observation == 0), ]),
            ties_prop = sum(temp$observation == 0) / nrow(temp),
            n_gaps = sum(abs(diff(temp$datetime)) >= 2)
        )
        # append to results
        results <- rbind(results, results_temp)
    }

    # write to file
    readr::write_csv(
        results,
        file = here::here(paste0(
            "./outputs/pre-wpe-neon-data-checks/",
            paste0(challenge, "pre-wpe-info.csv")
        ))
    )

    return(results)
}
