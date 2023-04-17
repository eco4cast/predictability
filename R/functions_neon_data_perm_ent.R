##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-04-11
#'
#' This file contains functions to calculate the permutation entropy for each of
#' the datasets in the EFI Neon Forecasting challenge
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

aquatics_daily_perm_ent <- function(all_neon_data, data_path, fig_path) {
  #' Calculate permutation entropy for the acquatics challenge
  #' 
  #' @description Do the various data chopping that should be done to get the 
  #' aquatics 
  #' 
  #' @param all_neon_data list. A list of all the different datasets from all
  #' the challenges
  #' @param data_path character. Where to save the outputs of the computation
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
  
  # split into the ones that are in the oxy/temp sites and the chla sites
  oxy_temp_sites <- c(unique(lakes$field_site_id), 
                      unique(river_streams$field_site_id))
  chla_sites <- c(unique(lakes$field_site_id),
                  unique(non_wade_river$field_site_id))
  
  unique(lakes$field_site_id)
  unique(river_streams$field_site_id)
  
  # do the filtering for a single site
  for(site in unique(lakes$field_site_id)) {
    df <- aquatics %>% dplyr::filter(site == site)
    
    if(df$site %in% oxy_temp_sites) {
      # determine which variables this site should get 
      df_oxy <- df %>% 
        dplyr::filter(variable == "oxygen")
      oxy_vec <- df_oxy$observation
      
      df_temp <- df %>% 
        dplyr::filter(variable == "tempurature") 
      temp_vec <- df_temp$observation
      
      oxy_npe <- 
      
    }
    ggplot(data = df %>% 
             dplyr::filter(variable == "oxygen")) + 
      geom_point(aes(x = datetime, y = observation))
  }
  
  
  
}
