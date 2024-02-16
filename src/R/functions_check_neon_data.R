##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2024-02-15
#'
#' This file contains functions to calculate get the important information
#' we need to know before we pass things through the PE calculation
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

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
#' make_groups(df, )
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
#'
#' @return None
#' @export
#' @examples
#' plot_gaps(df)
plot_gaps <- function(df, challenge) {

}

#' @title wpe_check_neon_data
#'
#' @description This function checks the data for the following:
#' 1) number of observations
#' 2) proportion of zeros in the time-series
#' 3) proportion of ties in the time-series
#' 4) gaps
#' This also writes a formatted table to file with the results
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
    site_ids <- unique(df$site_id)
    variables <- unique(df$variable)
    df_info <- data.frame(expand.grid(
        "site_ids" = site_ids,
        "variables" = variables
    ))

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
        temp <- df[which(df$site_id == df_info[row, "site_ids"] &
            df$variable == df_info[row, "variables"]), ]
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
