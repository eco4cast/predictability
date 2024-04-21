##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-04-11
#'
#' This file contains functions to calculate the permutation entropy for each of
#' the datasets in the EFI Neon Forecasting challenge
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

aquatics_daily_perm_ent <- function(all_neon_data, output_path, fig_path) {
  #' Calculate permutation entropy for the acquatics challenge
  #'
  #' @description Do the various data chopping that should be done to get the
  #' aquatics
  #'
  #' @param all_neon_data list. A list of all the different datasets from all
  #' the challenges
  #' @param output_path character. Where to save the outputs of the computation
  #' @param fig_path character. Where to save any figures generated
  #'
  #' @usage aquatics_daily_perm_ent(all_neon_data, data_path, fig_path)
  #' @return a named list containing all of the variations of the permutation
  #' entropy calculations

  aquatics <- all_neon_data$aquatic_daily

  # From the forecast challenge webpage
  # (https://projects.ecoforecast.org/neon4cast-docs/Aquatics.html), the
  # challenge is to produce forecasts of mean daily surface water temperature
  # and/or dissolved oxygen, and forecasts of chlorophyll for a subset of those
  # sites.
  #
  # So we'll divide the data up by the sites of interest, then calculate
  # the entropy for each of the timeseries as they are relevant to producing
  # the forecast itself

  aquatic_sites <- all_neon_data$site_data %>%
    dplyr::filter(aquatics == 1)


  # get the sites for temperature and/or dissolved oxygen
  lakes <- aquatic_sites %>%
    dplyr::filter(field_site_subtype == "Lake")
  river_streams <- aquatic_sites %>%
    dplyr::filter(field_site_subtype %in%
      c("Wadeable Stream", "Non-wadeable River"))
  non_wade_river <- aquatic_sites %>%
    dplyr::filter(field_site_subtype == "Non-wadeable River")

  # so if the river is non-wadable, or if the site is a lake, then it's all
  # three - if it's the others, then it's just temp and oxygen
  all_three <-
    unique(lakes$field_site_id)
  unique(river_streams$field_site_id)

  # do the filtering for a single site
  for (site in unique(lakes$field_site_id)) {
    df <- aquatics %>% dplyr::filter(site == site)

    if (site %in% non_wade_river | site %in% lakes) { # if true, calc all three

      # pull the two variables of oxygen and temperature for the sites at hand
      df_oxy <- df %>%
        dplyr::filter(variable == "oxygen")
      oxy_vec <- df_oxy$observation

      df_temp <- df %>%
        dplyr::filter(variable == "temperature")
      temp_vec <- df_temp$observation

      df_chla <- df %>%
        dplyr::filter(variable == "chla")
      chla_vec <- df_chla$observation

      # calc for all the
      oxy_npe <- perm_ent_calc(oxy_vec, dim_all = TRUE)
      temp_npe <- perm_ent_calc(temp_vec, dim_all = TRUE)
      chla_npe <- perm_ent_calc(chla_vec, dim_all = TRUE)

      perm_ent <- list(
        oxygen = oxy_npe,
        temperature = temp_npe,
        chla = chla_npe
      )

      # write out the rds of the file
      writeRDS(perm_ent, paste0())
    }
    ggplot(data = df %>%
      dplyr::filter(variable == "oxygen")) +
      geom_point(aes(x = datetime, y = observation))
  }
}
