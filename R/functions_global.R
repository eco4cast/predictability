##' AUTHOR: Cole Brookson
##' DATE OF CREATION: 2023-04-11
#'
#' This file contains all of the commonly used functions inside the whole repo
#' that will be needed across scripts
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library

# a plotting theme =============================================================

# Note that the below theme is almost identical to the theme_base provided in 
# the ggthemes() package (see original function here: 
# https://github.com/jrnold/ggthemes/blob/main/R/base.R
# ), but it removes the black rectangle around the whole
# plot



#' Foundation Theme
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation()}
#' is a complete theme with only minimal number of elements defined.
#' It is easier to create new themes by extending this one rather
#' than \code{\link[ggplot2]{theme_gray}()} or \code{\link[ggplot2]{theme_bw}()},
#' because those themes define elements deep in the hierarchy.
#'
#' This theme takes \code{\link[ggplot2]{theme_gray}()} and sets all
#' \code{colour} and \code{fill} values to \code{NULL}, except for the top-level
#' elements (\code{line}, \code{rect}, and \code{title}), which have
#' \code{colour = "black"}, and \code{fill = "white"}. This leaves the spacing
#' and-non colour defaults of the default \pkg{ggplot2} themes in place.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @export
#' @importFrom ggplot2 theme_grey
theme_foundation <- function(base_size=12, base_family="") {
  thm <- theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA),
              legend.background = element_rect(colour = NA),
              line = element_line(colour = "black"),
              rect = element_rect(fill = "white", colour = "black"),
              text = element_text(colour = "black"))
}




#' Theme Base
#'
#' Theme similar to the default settings of the \sQuote{base} R graphics.
#'
#' @inheritParams ggplot2::theme_bw
#' @export
#' @family themes
#' @example inst/examples/ex-theme_base.R
theme_base <- function(base_size = 16, base_family = "") {
  theme_foundation() +
    theme(line = element_line(colour = "black",
                              lineend = "round",
                              linetype = "solid"),
          rect = element_rect(fill = "white",
                              colour = "black",
                              linetype = "solid"),
          text = element_text(colour = "black",
                              face = "plain",
                              family = base_family,
                              size = base_size,
                              vjust = 0.5,
                              hjust = 0.5,
                              lineheight = 1),
          panel.grid = element_blank(),
          strip.background = element_rect(colour = NA),
          legend.key = element_rect(colour = NA),
          title = element_text(size = rel(1)),
          plot.title = element_text(size = rel(1.2), face = "bold"),
          strip.text = element_text(),
          axis.ticks.length = unit(0.5, "lines"),
          # add my addition here
          plot.background = element_rect(colour = NA)
    )
  # TODO: get margins right
}

# get_data =====================================================================
get_data_csv = function(file) {
  #' Takes in the file and reads it for use
  #' 
  #' @description Generic function to interface with the file targets, to read 
  #' them in and make them available for analysis
  #' 
  #' @param file character. The file path in the target
  #'  
  #' @usage get_data_csv(here("./data/wild-lice/file.csv"))
  #' @return Dataframe 
  #' 
  
  readr::read_csv(file, show_col_types = FALSE) 
}