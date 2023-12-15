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
autocorr_plot <- function(obs, data_name, sub_folder, site=NULL) {
  
  if(!is.null(site)) {
    png(
      paste0(here::here("./figs/neon-autocorrelation-plots/", sub_folder, "/"),
              "acf-", data_name, "-", site, ".png"),
    )
    acf(obs,pl=TRUE)
    
    png(
      paste0(here::here("./figs/neon-autocorrelation-plots/", sub_folder, "/"),
             "partial-", data_name, "-", site, ".png"),
    )
    pacf(obs,pl=TRUE)
    dev.off()
  }
  
  png(
    paste0(here::here("./figs/neon-autocorrelation-plots/", sub_folder, "/"),
           data_name, ".png"),
  )
  acf(obs,pl=TRUE)
  
  png(
    paste0(here::here("./figs/neon-autocorrelation-plots/", sub_folder, "/"),
           "partial-", data_name, ".png"),
  )
  pacf(obs,pl=TRUE)
  
  # having weird problem with devices so trying to fix that here
  for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
    dev.off()
  }
}

#' Get the CI of a ACF function
#' 
#' @description
#' The actual plus/minus value of the CI (assuming 95%) from an autocorrelation
#' function
#' 
#' @param x vector. The numeric vector to calculate based on
#' @param ci double. Defaul = 0.95, denoting the CI level
#' @param ci.type character. Two options, "white" denoting white noise 
#' hypothesis or "ma" which is the moving average
#' 
#' @return clim0 numeric. The value for upper and lower
get_clim <- function(x, ci=0.95, ci.type="white"){
  #' Gets confidence limit data from acf object `x`
  if (!ci.type %in% c("white", "ma")) stop('`ci.type` must be "white" or "ma"')
  if (class(x) != "acf") stop('pass in object of class "acf"')
  clim0 <- qnorm((1 + ci)/2) / sqrt(x$n.used)
  if (ci.type == "ma") {
    clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1]^2))) 
    return(clim[-length(clim)])
  } else {
    return(clim0)
  }
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
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  all_sites_pacf <- vector(mode = "list", length = 
                             length(unique(ticks$site_id)))
  all_sites_acf <- vector(mode = "list", length = 
                            length(unique(ticks$site_id)))
  all_sites_pacf_ci <- vector(mode = "list", length = 
                                length(unique(ticks$site_id)))
  all_sites_acf_ci <- vector(mode = "list", length = 
                              length(unique(ticks$site_id)))
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
    ggsave(paste0(here::here("./figs/neon-data-timeseries/ticks-by-site/"),
             "ticks-", site, ".png"),
      ticks_site_plot)
    
    # now take the observations and do the plotting function to the get the vals
    obs <- ticks_site$observation
    all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
    all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
    all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
    all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
    
    # iterate and plot the outputs
    site_num <- site_num + 1
    autocorr_plot(obs, "ticks", site = site, sub_folder = "ticks-by-site")
  }
  
  # now take the averages and plot those
  mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
  ci <- 1.96*std_err(mean_pacf)
  lags <- c(1:18)
  
  # plot the partial averages
  png(
    paste0(
      here::here(
        "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
      "pacf-mean-ticks", ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
         (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
  dev.off()
  
  # plot the regular averages
  png(
    paste0(
      here::here(
        "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
      "acf-mean-ticks", ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
  dev.off()
  
}

#' Daily Terrestrial carbon autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the terrestrial daily data 
#' @param terr_day data frame. Data of all tick abundance across sites
#' 
#' @return none. instead of return, two plots are written to file
terr_day_autocor <- function(terr_day) {
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  for(ob_type in unique(terr_day$variable)) {
    
    terr_day_ob <- terr_day[
      which(terr_day$variable == ob_type), 
    ]
    
    all_sites_pacf <- vector(mode = "list", length = 
                               length(unique(terr_day_ob$site_id)))
    all_sites_acf <- vector(mode = "list", length = 
                              length(unique(terr_day_ob$site_id)))
    all_sites_pacf_ci <- vector(mode = "list", length = 
                                  length(unique(terr_day_ob$site_id)))
    all_sites_acf_ci <- vector(mode = "list", length = 
                                 length(unique(terr_day_ob$site_id)))
    site_num <- 1
    
    # go through each site and get the values then plot them
    for(site in unique(terr_day_ob$site_id)) {
      
      terr_day_site <- terr_day_ob[which(terr_day_ob$site_id == site), ]
      
      # site by site plot
      terr_day_site_plot <- ggplot2::ggplot(data = terr_day_site) + 
        ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
        ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                            size = 2, fill = "red3", shape = 21) +
        theme_base() + 
        labs(y = ob_type, x = "Time")
      ggsave(paste0(
        here::here("./figs/neon-data-timeseries/terr-daily-by-site/"),
                    "terr-daily-", ob_type, "-", site, ".png"),
             terr_day_site_plot)
      
      obs<-terr_day_site$observation[which(terr_day_site$variable == ob_type)]
      
      all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
      all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
      all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
      all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
      
      #plot the outputs
      autocorr_plot(obs, ob_type, site = site, 
                    sub_folder = "terr-daily-by-site")
      # iterate and plot the outputs
      site_num <- site_num + 1
      }

    
    # now take the averages and plot those
    mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_pacf)
    lags <- c(1:18)
    
    # plot the partial averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "pacf-mean-terr-", ob_type , ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
                  (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
    
    # plot the regular averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "acf-mean-mean-terr-", ob_type, ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
  }
}

#' 30min terrestrial carbon autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the terrestrial daily data 
#' @param terr_30 data frame. Data of all tick abundance across sites
#' 
#' @return none. instead of return, two plots are written to file
terr_30_autocor <- function(terr_30) {
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  for(ob_type in unique(terr_30$variable)) {
    
    terr_30_ob <- terr_30[
      which(terr_30$variable == ob_type), 
    ]
    
    all_sites_pacf <- vector(mode = "list", length = 
                               length(unique(terr_30_ob$site_id)))
    all_sites_acf <- vector(mode = "list", length = 
                              length(unique(terr_30_ob$site_id)))
    all_sites_pacf_ci <- vector(mode = "list", length = 
                                  length(unique(terr_30_ob$site_id)))
    all_sites_acf_ci <- vector(mode = "list", length = 
                                 length(unique(terr_30_ob$site_id)))
    site_num <- 1
    
    # go through each site and get the values then plot them
    for(site in unique(terr_30_ob$site_id)) {
      
      terr_30_site <- terr_30_ob[which(terr_30_ob$site_id == site), ]
      
      # site by site plot
      terr_30_site_plot <- ggplot2::ggplot(data = terr_30_site) + 
        ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
        ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                            size = 2, fill = "red3", shape = 21) +
        theme_base() + 
        ggplot2::labs(y = ob_type, x = "Time")
      ggplot2::ggsave(paste0(
        here::here("./figs/neon-data-timeseries/terr-30min-by-site/"),
        "terr-30min-", ob_type, "-", site, ".png"),
        terr_30_site_plot)
      
      obs <- terr_30_site$observation[
        which(terr_30_site$variable == ob_type & !is.na(
          terr_30_site$observation))
        ]
      
      all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
      all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
      all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
      all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
      
      #plot the outputs
      autocorr_plot(obs, ob_type, site = site, 
                    sub_folder = "terr-30min-by-site")
      # iterate and plot the outputs
      site_num <- site_num + 1
      print(site)
    }
    
    # now take the averages and plot those
    mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_pacf)
    lags <- c(1:18)
    
    # plot the partial averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "pacf-mean-terr30min-", ob_type , ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
                  (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
    
    # plot the regular averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "acf-mean-mean-terr30min-", ob_type, ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
  }
}

#' Aquatic daily autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the aquatic daily data 
#' @param aquat_day data frame. Data of all tick abundance across sites
#' 
#' @return none. instead of return, two plots are written to file
aquat_day_autocor <- function(aquat_day) {
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  for(ob_type in unique(aquat_day$variable)) {
    
    aquat_day_ob <- aquat_day[
      which(aquat_day$variable == ob_type), 
    ]
    
    all_sites_pacf <- vector(mode = "list", length = 
                               length(unique(aquat_day_ob$site_id)))
    all_sites_acf <- vector(mode = "list", length = 
                              length(unique(aquat_day_ob$site_id)))
    all_sites_pacf_ci <- vector(mode = "list", length = 
                                  length(unique(aquat_day_ob$site_id)))
    all_sites_acf_ci <- vector(mode = "list", length = 
                                 length(unique(aquat_day_ob$site_id)))
    site_num <- 1
    
    # go through each site and get the values then plot them
    for(site in unique(aquat_day_ob$site_id)) {
      
      aquat_day_site <- aquat_day_ob[which(aquat_day_ob$site_id == site), ]
      
      # site by site plot
      aquat_day_site_plot <- ggplot2::ggplot(data = aquat_day_site) + 
        ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
        ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                            size = 2, fill = "red3", shape = 21) +
        theme_base() + 
        ggplot2::labs(y = ob_type, x = "Time")
      ggsave(paste0(
        here::here("./figs/neon-data-timeseries/aquatics-daily-by-site/"),
        "aquat-daily-", ob_type, "-", site, ".png"),
        aquat_day_site_plot)
      
      obs<-aquat_day_site$observation[which(aquat_day_site$variable == ob_type)]
      
      all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
      all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
      all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
      all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
      
      #plot the outputs
      autocorr_plot(obs, ob_type, site = site, 
                    sub_folder = "aquatics-daily-by-site")
    }
    # iterate and plot the outputs
    site_num <- site_num + 1
    
    # now take the averages and plot those
    mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_pacf)
    lags <- c(1:18)
    
    # plot the partial averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "pacf-mean-aquat-day-", ob_type , ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
                  (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
    
    # plot the regular averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "acf-mean-aquat-day-", ob_type, ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
  }
}

#' Aquatic daily autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the aquatic daily data 
#' @param aquat_hour data frame. Data of all tick abundance across sites
#' 
#' @return none. instead of return, two plots are written to file
aquat_hour_autocor <- function(aquat_hour) {
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  for(ob_type in unique(aquat_hour$variable)) {
    
    aquat_hour_ob <- aquat_hour[
      which(aquat_hour$variable == ob_type), 
    ]
    
    all_sites_pacf <- vector(mode = "list", length = 
                               length(unique(aquat_hour_ob$site_id)))
    all_sites_acf <- vector(mode = "list", length = 
                              length(unique(aquat_hour_ob$site_id)))
    all_sites_pacf_ci <- vector(mode = "list", length = 
                                  length(unique(aquat_hour_ob$site_id)))
    all_sites_acf_ci <- vector(mode = "list", length = 
                                 length(unique(aquat_hour_ob$site_id)))
    site_num <- 1
    
    # go through each site and get the values then plot them
    for(site in unique(aquat_hour_ob$site_id)) {
      
      aquat_hour_site <- aquat_hour_ob[which(aquat_hour_ob$site_id == site), ]
      
      # site by site plot
      aquat_hour_site_plot <- ggplot2::ggplot(data = aquat_hour_site) + 
        ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
        ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                            size = 2, fill = "red3", shape = 21) +
        theme_base() + 
        ggplot2::labs(y = ob_type, x = "Time")
      ggsave(paste0(
        here::here("./figs/neon-data-timeseries/aquatics-hourly-by-site/"),
        "aquat-hourly-", ob_type, "-", site, ".png"),
        aquat_hour_site_plot)
      
      obs<-aquat_hour_site$observation[which(aquat_hour_site$variable == ob_type)]
      
      all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
      all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
      all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
      all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
      
      #plot the outputs
      autocorr_plot(obs, ob_type, site = site, 
                    sub_folder = "aquatics-hourly-by-site")
    }
    # iterate and plot the outputs
    site_num <- site_num + 1
    
    # now take the averages and plot those
    mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_pacf)
    lags <- c(1:18)
    
    # plot the partial averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "pacf-mean-aquat-hourly-", ob_type , ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
                  (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
    
    # plot the regular averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "acf-mean-aquat-hourly-", ob_type, ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
  }
}

#' Get autocorrelation and partial autocorrelation
#' 
#' @description
#' Calculate values of autocorrelation and partial autocorrelation for all of 
#' the variable types present
#' @param data data frame. The standard EFI-NEON dataset
#' @param subfolder character. Which sub-folder to save the plots in
#' @param datatype character. Which of the data objects to use 
#' 
#' @return none. instead of return, two plots are written to file
autocorr <- function(data) {
  
  # use some vectors to store the calculations across all sites so we can get 
  # an average across all sites
  for(ob_type in unique(data$variable)) {
    
    data_ob <- data[
      which(data$variable == ob_type), 
    ]
    
    all_sites_pacf <- vector(mode = "list", length = 
                               length(unique(data_ob$site_id)))
    all_sites_acf <- vector(mode = "list", length = 
                              length(unique(data_ob$site_id)))
    all_sites_pacf_ci <- vector(mode = "list", length = 
                                  length(unique(data_ob$site_id)))
    all_sites_acf_ci <- vector(mode = "list", length = 
                                 length(unique(data_ob$site_id)))
    site_num <- 1
    
    # go through each site and get the values then plot them
    for(site in unique(data_ob$site_id)) {
      
      data_site <- data_ob[which(data_ob$site_id == site), ]
      
      # site by site plot
      data_site_plot <- ggplot2::ggplot(data = data_site) + 
        ggplot2::geom_line(ggplot2::aes(x = datetime, y = observation)) + 
        ggplot2::geom_point(ggplot2::aes(x = datetime, y = observation),
                            size = 2, fill = "red3", shape = 21) +
        theme_base() + 
        ggplot2::labs(y = ob_type, x = "Time")
      ggsave(paste0(
        here::here("./figs/neon-data-timeseries/", subfolder, "/"),
        datatype, ob_type, "-", site, ".png"),
        data_site_plot)
      
      obs<-data_site$observation[which(data_site$variable == ob_type)]
      
      all_sites_pacf[[site_num]] <- pacf(obs, pl = FALSE)$acf[1:18]
      all_sites_pacf_ci[[site_num]] <- get_clim(pacf(obs, pl = FALSE))
      all_sites_acf[[site_num]] <- acf(obs, pl = FALSE)$acf[1:18]
      all_sites_acf_ci[[site_num]] <- get_clim(acf(obs, pl = FALSE))
      
      #plot the outputs
      autocorr_plot(obs, ob_type, site = site, 
                    sub_folder = subfolder)
    }
    # iterate and plot the outputs
    site_num <- site_num + 1
    
    # now take the averages and plot those
    mean_pacf <- colMeans(do.call(rbind, all_sites_pacf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_pacf)
    lags <- c(1:18)
    
    # plot the partial averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "pacf-mean-", datatype, "-", ob_type , ".png"))
    plot(x = lags, y = mean_pacf, type = "h", 
         ylim = c(-(max(mean_pacf)+0.5*max(mean_pacf)), 
                  (max(mean_pacf)+0.5*max(mean_pacf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
    
    # plot the regular averages
    png(
      paste0(
        here::here(
          "./figs/neon-autocorrelation-plots/mean-autocorrelation/"),
        "acf-mean-", datatype, "-", ob_type, ".png"))
    mean_acf <- colMeans(do.call(rbind, all_sites_acf), na.rm = TRUE)
    ci <- 1.96*std_err(mean_acf)
    lags <- c(1:18)
    
    plot(x = lags, y = mean_acf, type = "h", 
         ylim = c(-(max(mean_acf)+0.5*max(mean_acf)), 
                  (max(mean_acf)+0.5*max(mean_acf))),
         xlab = "Lags", ylab = "Mean PACF")
    abline(h = 0, col = "grey80", lty = 2)
    abline(h = ci, col = "red2", lty = 2)
    abline(h = -ci, col = "red2", lty = 2)
    dev.off()
  }
}