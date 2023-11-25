##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-11-22
#'
#' This file contains functions to calculate the autocorrelation in each of the 
#' datasets from the EFI-NEON forecasting challenge
#' 
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

#' Plot autocorrelation
#' 
#' @description
#' We need a plot of each of the autocorrelation plots, ideally one for each 
#' site that we're interested in
#' @param obs vector. The vector of observations we're doing the calculation
#' on
#' @param data_name character. The character denoting which data we're actually
#' plotting 
#' @param site character. Default NULL. This denotes which sxite the obs are for
#' since most will have a differnt set of sites for each dataframe
#' 
#' @return none. instead of return, two plots are written to file
autocorr_plot <- function(obs, data_name, site=NULL) {
  
  if(!is.null(site)) {
    png(
      paste0(here::here("./figs/neon-autocorrelation-plots/"),
              data_name, "-", site, ".png"),
    )
    acf(obs,pl=TRUE)
    
    png(
      paste0(here::here("./figs/neon-autocorrelation-plots/"),
             "partial-", data_name, "-", site, ".png"),
    )
    pacf(obs,pl=TRUE)
    dev.off()
  }
  
  png(
    paste0(here::here("./figs/neon-autocorrelation-plots/"),
           data_name, ".png"),
  )
  acf(obs,pl=TRUE)
  
  png(
    paste0(here::here("./figs/neon-autocorrelation-plots/"),
           "partial-", data_name, ".png"),
  )
  pacf(obs,pl=TRUE)
  dev.off()
}

#' Ticks autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the tick data 
#' @param ticks data frame. Data of all tick abundance across sites
#' 
#' @return none. instead of return, two plots are written to file
ticks_autocor <- function(ticks) {
  # use some vectors to store the calculations across all sites so we can geet 
  # an average
  all_sites_pacf <- vector(mode = "list", 
                           length = length(unique(ticks$site_id)))
  site_num <- 1
  # go through each site and get the values then plot them
  for(site in unique(ticks$site_id)) {
    ticks_site <- ticks[which(ticks$site_id == site), ]
    # site by site plot
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
    # now take the observations and do the plotting function to the get the vals
    obs <- ticks_site$observation
    all_sites[[site_num]] <- obs
    site_num <- site_num + 1
    #autocorr_plot(obs, "ticks", site = site)
  }
  partial_all_sites <- sapply(all_sites, pacf, pl=FALSE)
}







































