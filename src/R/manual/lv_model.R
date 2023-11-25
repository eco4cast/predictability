# set up 

library(tidyverse)
library(deSolve)
library(FME)
library(here)
source(here("./R/functions_global.R"))

# the models for reference: 
# dx/dt = alpha*x - beta*x*y
# dy/dt = delta*beta*x*y - gamma*y

# initial parameters
pars <- c(alpha = 1, beta = 0.2, delta = 0.5, gamma = 0.2)
init <- c(x = 1, y = 2) #initial state
times <- seq(0, 100, 0.1)



# wrapper so we can get a sensitivity analysis going 

#' Lotka-Volterra equation solver with a wrapper
#' 
#' @description The lotka-volterra equation as `lv_model`, wrapped inside a 
#' larger function to facilitate a sensitivity analysis. 
#' 
#' @param parameters named numeric vector. The parameters needed for the model 
#' to run. Should be a named numeric vector of length 4 
#' @param state named numeric vector. The initial state values of the system. 
#' Should be named "x" and "y" respectively and passed as integers or numeric
#' values. DEFAULT is c(x = 1, y = 2)
#' @param times numeric vector.  DEFAULT is c(x = 1, y = 2)
#' @returns A numeric vector.
#' @examples
#' lv_fme_wrapper()
#' add(10, 1)
lv_fme_wrapper <- function(parameters, state = c(x = 1, y = 2), 
                           times = seq(0, 100, 0.1)) {
  
  # do type checks 
  if(!is.numeric(parameters)) {
    errorCondition("ERROR - parameters not a numeric vector")
  }
  if(any(names(parameters) != c("alpha", "beta", "delta", "gamma"))) {
    errorCondition("ERROR - names of parameters incorrect")
  }
  
  # set initial conditions
  state <- c(x = 1, y = 2)
  # get the derrivative
  lv_model <- function(times, state, parameters) { 
    with(as.list(c(state, parameters)), { 
      d_x <- alpha * x - beta * x * y
      d_y <- delta * beta * x * y - gamma * y
      return(list(c(x = d_x, y = d_y)))
    })
  }
  # do the solve within the wrapper
  deSolve::lsoda(y = state, times = times, func = lv_model, 
                 parms = parameters)
}

# initial run
lv_results <- lv_fme_wrapper(
  parameters = pars
)
# https://strimas.com/post/lotka-volterra/
lv_results %>% 
  data.frame() %>% 
  gather(var, pop, -time) %>% 
  mutate(var = if_else(var == "x", "Prey", "Predator")) %>% 
  ggplot(aes(x = time, y = pop)) +
  geom_line(aes(color = var)) +
  scale_color_brewer(NULL, palette = "Set1") +
  labs(title = "Lotka-Volterra predator prey model",
       subtitle = paste(names(pars), pars, sep = " = ", collapse = "; "),
       x = "Time", y = "Population density") +
  theme_base()

