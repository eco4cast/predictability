# set up 

library(tidyverse)
library(deSolve)
library(FME)

# the models for reference: 
# dx/dt = alpha*x - beta*x*y
# dy/dt = delta*beta*x*y - gamma*y

# initial parameters
pars <- c(alpha = 1, beta = 0.2, delta = 0.5, gamma = 0.2)
init <- c(x = 1, y = 2) #initial state
times <- seq(0, 100, 0.1)



# wrapper so we can get a sensitivity analysis going 
lv_fme_wrapper <- function(parameters, times = seq(0, 100, 0.1)) {
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
  deSolve::lsoda(y = state, times = times, func = lv_model, parameters = pars)
}