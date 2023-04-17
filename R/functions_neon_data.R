##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-25
#'
#' This file contains functions that read in and save the data from the EFI
#' NEON Forecasting Challenge (https://projects.ecoforecast.org/neon4cast-docs/)
#' and then does the necessary data cleaning to get them ready to be used in
#' analysis of permutation entropy
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# pull online data from source =================================================
pull_data <- function(write = FALSE) {
  #' Get all 5 themes from the website
  #' 
  #' @description Using the targets already prepared by EFI, read in the 
  #' data as data.frame types, and then save them to a local file each 
  #' 
  #' @param write boolean. Default is FALSE, if TRUE, the files when pulled from
  #' online will be written to the local machine
  #' 
  #' @usage pull_data
  #' @return no returns, but writes out 5 data files
  
  ## pull the aquatics challenge ===============================================
  aquatic_daily <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-targets.csv.gz")) |> 
    na.omit()
  aquatic_hourly <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "aquatics/aquatics-expanded-observations.csv.gz")) |> 
    na.omit()
  
  ## pull terrestrial data =====================================================
  terr_30_min <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "terrestrial_30min/terrestrial_30min-targets.csv.gz"), 
          guess_max = 1e6)
  terr_daily <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "terrestrial_daily/terrestrial_daily-targets.csv.gz"), 
          guess_max = 1e6) |> 
    na.omit()
  
  ## pull ticks data ===========================================================
  ticks <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
          "ticks/ticks-targets.csv.gz"),
    guess_max = 1e6)
  
  ## pull phenology data =======================================================
  phenology <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "phenology/phenology-targets.csv.gz"),
    guess_max = 1e6) |> 
    na.omit()
  
  ## pull beetles data =========================================================
  beetles <- readr::read_csv(
    paste0("https://data.ecoforecast.org/neon4cast-targets/",
           "beetles/beetles-targets.csv.gz"),
    guess_max = 1e6)

  # if write is TRUE, then write the data to files 
  if(write == TRUE) {
    readr::write_csv(
      x = aquatic_daily,
      file = here::here("./data/efi-neon-data/aquatic-daily.csv")
    )
    readr::write_csv(
      x = aquatic_hourly,
      file = here::here("./data/efi-neon-data/aquatic-hourly.csv")
    )
    readr::write_csv(
      x = terr_daily,
      file = here::here("./data/efi-neon-data/terrestrial-daily.csv")
    )
    readr::write_csv(
      x = terr_30_min,
      file = here::here("./data/efi-neon-data/terrestrial-30-mins.csv")
    )
    readr::write_csv(
      x = ticks,
      file = here::here("./data/efi-neon-data/ticks.csv")
    )
    readr::write_csv(
      x = phenology,
      file = here::here("./data/efi-neon-data/phenology.csv")
    )
    readr::write_csv(
      x = beetles,
      file = here::here("./data/efi-neon-data/beetles.csv")
    )
  }
  
  listed_res <- list(aquatic_daily, aquatic_hourly, terr_30_min, terr_daily, 
                     phenology, ticks, beetles)
  names(listed_res) <- c("aquatic_daily", "aquatic_hourly", 
                         "terrestrial_30mins", "terrestrial_daily", 
                         "phenology", "ticks", "beetles")
  
  return(listed_res)
}

# plot all timeseries ==========================================================
plot_timeseries <- function(listed_res, output_path) {
  #' Plot all useful timeseries from the NEON forecasting challenge
  #' 
  #' @description For simple visualization purposes, plot the different 
  #' timeseries and save them to files 
  #' 
  #' @param listed_res list. A list of all the downloaded NEON data.
  #' @param output_path character. Where to save the files 
  #' 
  #' @usage plot_neon_timeseries(listed_res)
  #' @return no returns, but writes out 5 data files
  
  ## daily terrestrial =========================================================
  terr_day <- listed_res$terrestrial_daily 
  
  # split into the two variable types - do the le one first
  le_day <- terr_day %>% 
    dplyr::filter(variable == "le") %>% 
    dplyr::mutate(le = observation) %>% 
    dplyr::select(-variable) 
  le_avg_day <- le_day %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_le = mean(le, na.rm = TRUE))
  
  p_le_day <- ggplot() + 
    geom_point(data = le_day, 
               aes(x = datetime, y = le), 
               shape = 21, alpha = 0.02, fill = "red") +
    geom_line(data = le_avg_day, 
              aes(x = datetime, y = mean_le)) + 
    theme_base() + 
    labs(x = "Date", y = "Latent Heat Flux (W m<sup>-2</sup>)") +
    theme(
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown()
    ) + 
    ylim(c(-10, 250))
  
  ggplot2::ggsave(
    # filename
    paste0(output_path, "terr-daily-latent-heat-flux.png"),
    # plot
    p_le_day
  )
  
  # now do with the other variable
  nee_day <- terr_day %>% 
    dplyr::filter(variable == "nee") %>% 
    dplyr::mutate(nee = observation) %>% 
    dplyr::select(-variable) 
  nee_avg_day <- nee_day %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_nee = mean(nee, na.rm = TRUE))
  
  p_nee_day <- ggplot() + 
    geom_point(data = nee_day, 
               aes(x = datetime, y = nee), 
               shape = 21, alpha = 0.02, fill = "blue") +
    geom_line(data = nee_avg_day, 
              aes(x = datetime, y = mean_nee)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Net ecosystem exchange (g C m<sup>-2</sup> day<sup>-1</sup>)") +
    theme(
      axis.title.y = element_markdown()
    )
  
  # make the plot
  ggplot2::ggsave(
    # file name
    paste0(output_path, "terr-daily-net-eco-exchange.png"),
    # the plot
    p_nee_day
  )
  
  # make one of both plots side by side 
  p_terr_day <- p_le_day + p_nee_day
  
  ggplot2::ggsave(
    paste0(output_path, "terr-daily-all-vars.png"),
    p_terr_day
  )
  
  ## terrestrial thirty minutes ================================================
  
  ## aquatic daily =============================================================
  aquatic_daily <- listed_res$aquatic_daily
  
  # for oxygen first 
  oxy_day <- aquatic_daily %>% 
    dplyr::filter(variable == "oxygen")
  mean_oxy_day <- oxy_day %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_oxy = mean(observation, na.rm = TRUE))
  
  p_oxy <- ggplot() + 
    geom_point(data = oxy_day, 
               aes(x = datetime, y = observation), 
               shape = 21, alpha = 0.02, fill = "grey80") +
    geom_line(data = mean_oxy_day, 
              aes(x = datetime, y = mean_oxy)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Surface Mean Daily Dissolved Oxygen (mg L<sup>-1</sup>)") +
    theme(
      axis.title.y = element_markdown()
    ) 
  
  ggplot2::ggsave(
    paste0(output_path, "aquatic-daily-dis-oxy.png"),
    p_oxy
  )
  
  # now for temperature 
  temp_day <- aquatic_daily %>% 
    dplyr::filter(variable == "temperature")
  mean_temp_day <- temp_day %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_temp = mean(observation, na.rm = TRUE))
  
  p_temp <- ggplot() + 
    geom_point(data = temp_day, 
               aes(x = datetime, y = observation), 
               shape = 21, alpha = 0.02, fill = "orange") +
    geom_line(data = mean_temp_day, 
              aes(x = datetime, y = mean_temp)) + 
    theme_base() + 
    labs(x = "Date", y = "Surface Mean Daily Water Temperature (°C)") +
    theme(
      axis.title.y = element_markdown()
    ) 
  
  ggplot2::ggsave(
    paste0(output_path, "aquatic-daily-surf-temp.png"),
    p_temp
  )
  
  # chlorophyll
  chla_day <- aquatic_daily %>% 
    dplyr::filter(variable == "chla")
  mean_chla_day <- chla_day %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_chla = mean(observation, na.rm = TRUE))
  
  p_chla <- ggplot() + 
    geom_point(data = chla_day, 
               aes(x = datetime, y = observation), 
               shape = 21, alpha = 0.02, fill = "green3") +
    geom_line(data = mean_chla_day, 
              aes(x = datetime, y = mean_chla)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Daily Mean Chlorophyll-a Concentration (mg L<sup>-1</sup>)") +
    theme(
      axis.title.y = element_markdown()
    )  + 
    ylim(c(0, 75))
  
  ggplot2::ggsave(
    paste0(output_path, "aquatic-daily-chla-conc.png"),
    p_chla
  )
  
  # put all variables together for one plot 
  p_aquat_daily <- p_oxy + p_temp + p_chla
  
  ggplot2::ggsave(
    paste0(output_path, "aquatic-daily-all-vars.png"),
    p_aquat_daily
  )
  
  ## aquatic hourly ============================================================
  
  ## beetles ===================================================================
  beetles <- listed_res$beetles
  
  # abundance first
  abund_beetle <- beetles %>% 
    dplyr::filter(variable == "abundance") 
  mean_abund_beetle <- abund_beetle %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_abund = mean(observation, na.rm = TRUE))
  
  p_abund <- ggplot(data = abund_beetle) + 
    geom_point(aes(x = datetime, y = observation),
               shape = 21, alpha = 0.1, fill = "purple") + 
    geom_line(data = mean_abund_beetle, aes(x = datetime, y = mean_abund)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Total number of carabids per-trap-night") +
    theme(
      axis.title.y = element_markdown()
    ) +
    ylim(c(0,5))
  
  ggplot2::ggsave(
    paste0(output_path, "beetles-abundance.png"),
    p_abund
  )
  
  # richness in species 
  rich_beetle <- beetles %>% 
    dplyr::filter(variable == "abundance") 
  mean_rich_beetle <- abund_beetle %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_abund = mean(observation, na.rm = TRUE))
  
  p_rich <- ggplot(data = rich_beetle) + 
    geom_point(aes(x = datetime, y = observation),
               shape = 21, alpha = 0.1, fill = "maroon") + 
    geom_line(data = mean_abund_beetle, aes(x = datetime, y = mean_abund)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Total number of carabids per-trap-night") +
    theme(
      axis.title.y = element_markdown()
    ) + 
    ylim(c(0,6))
  
  ggplot2::ggsave(
    paste0(output_path, "beetles-richness.png"),
    p_rich
  )
  
  # put the variables together 
  p_beetles <- p_abund + p_rich 
  
  ggplot2::ggsave(
    paste0(output_path, "beetles-all-vars.png"),
    p_beetles
  )
  
  ## phenology =================================================================
  phenology <- listed_res$phenology
  
  # green chromatic first 
  gcc_90 <- phenology %>% 
    dplyr::filter(variable == "gcc_90") 
  mean_gcc_90 <- gcc_90 %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_gcc_90 = mean(observation, na.rm = TRUE))
  
  p_gcc <- ggplot(data = gcc_90) + 
    geom_point(aes(x = datetime, y = observation),
               shape = 21, alpha = 0.02, fill = "green") + 
    geom_line(data = mean_gcc_90, aes(x = datetime, y = mean_gcc_90)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Green chromatic coordinate (90th percentile)") +
    theme(
      axis.title.y = element_markdown()
    ) 
  ggplot2::ggsave(
    paste0(output_path, "phenology-gcc.png"),
    p_gcc
  )
  
  # red chromatic now
  rcc_90 <- phenology %>% 
    dplyr::filter(variable == "rcc_90") 
  mean_rcc_90 <- rcc_90 %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_rcc_90 = mean(observation, na.rm = TRUE))
  
  p_rcc <- ggplot(data = rcc_90) + 
    geom_point(aes(x = datetime, y = observation),
               shape = 21, alpha = 0.02, fill = "red") + 
    geom_line(data = mean_rcc_90, aes(x = datetime, y = mean_rcc_90)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Red chromatic coordinate (90th percentile)") +
    theme(
      axis.title.y = element_markdown()
    ) 
  ggplot2::ggsave(
    paste0(output_path, "phenology-rcc.png"),
    p_rcc
  )
  
  # add plots together
  p_phenology <- p_gcc + p_rcc 
  ggplot2::ggsave(
    paste0(output_path, "phenology-all-vars.png"),
    p_phenology
  )
  
  # ticks ======================================================================
  ticks <- listed_res$ticks
  
  mean_ticks <- ticks %>% 
    dplyr::group_by(datetime) %>% 
    dplyr::summarize(mean_ticks = mean(observation, na.rm = TRUE))
  
  p_ticks <- ggplot(data = ticks) + 
    geom_point(aes(x = datetime, y = observation),
               shape = 21, alpha = 0.2, fill = "blue4") + 
    geom_line(data = mean_ticks, aes(x = datetime, y = mean_ticks)) + 
    theme_base() + 
    labs(x = "Date", 
         y = "Weekly density of  nymphs (ticks/1.6km<sup>2</sup>)") +
    theme(
      axis.title.y = element_markdown()
    ) 
  ggplot2::ggsave(
    paste0(output_path, "ticks.png"),
    p_ticks
  )
}