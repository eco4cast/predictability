# Predictability simulations
# Simulate a time series from a random walk model
# Two (three) measures of predictability

######## Random walk model #########
set.seed(14)
# Set up parameters
x_ic <- 10
tau_ic <- 1
a_obs <- 2
r_obs <- 1
a_add <- 2
r_add <- 1
nt <- 1000

# Initial conditions
x_0 <- rnorm(1, x_ic, tau_ic)
tau_add <- 1 / rgamma(1, a_add, r_add)
tau_obs <- 1 / rgamma(1, a_obs, r_obs)

# simulate x
x_t <- matrix(0, nrow = nt, ncol = 1)
x_t[1] <- x_0
for (i in 2:nt) x_t[i] <- rnorm(1, x_t[i - 1], tau_add)

# simulate y
y_t <- matrix(0, nrow = nt, ncol = 1)
for (i in 1:nt) y_t[i] <- rnorm(1, x_t[i], tau_obs)

# Clim
m_clim <- mean(y_t)
sd_clim <- sd(y_t)

# Relative entropy each time step for the next
D_t <- matrix(0, nrow = nt, ncol = 1)

# for (i in 2:nt) {
#   D_t[i] <- 0.5 * (log(tau_add / tau_obs) +
#     (tau_obs / tau_add) + ((y_t[i] - y_t[i - 1])^2) / tau_add - 1)
# }

# If treating y_t as a forecast, with y_0 as the last observed point,

D_t2 <- matrix(0, nrow = nt, ncol = 1)

for (i in 2:nt) {
  D_t2[i] <- 0.5 * (log(sd_clim / tau_obs) +
    (tau_obs / sd_clim) + ((m_clim - y_t[i])^2) / sd_clim - 1)
}
# plot(1:nt, D_t, type = "l")
plot(1:nt, D_t2, type = "l", ylab = "K-L Divergence")

# Start here ----

# Observed time series - AR1 process
ar1 <- .8 # From .8 to 1
ic0 <- 1
sd <- .1

y1 <- rnorm(1, mean = ar1 * ic0, sd = 1)

nt <- 1000
y_t <- matrix(0, nrow = nt)
y_t[1] <- y1

for (i in 2:nt) y_t[i] <- rnorm(1, mean = ar1 * y_t[i - 1], sd = 1)
plot(y_t, type = "l")

# Forecast distribution
ft <- 1000
mod_fit <- ar(y_t2)
m_fit1 <- mod_fit$ar[1]
m_fit2 <- mod_fit$ar[2]
m_fit3 <- mod_fit$ar[3]
sd_fit <- mod_fit$var.pred

y_tau <- matrix(0, nrow = ft)
y_tau[1] <- y_t2[249]
y_tau[2] <- y_t2[250]
# y_tau[3] <- y_t2[250]
m_tau <- matrix(0, nrow = ft, ncol = 1)
sd_tau <- matrix(0, nrow = ft, ncol = 1)
prior_sd <- 2
prior_mean <- 0
yt_mean <- mean(y_t2)
yt_sd <- sd(y_t2)

for (i in 3:ft) {
  ym_tau <- m_fit1 * y_tau[i - 1] + m_fit2 * y_tau[i - 2]
  b <- ((1 / prior_sd^2) * prior_mean) + ((nt / yt_sd^2) * ym_tau)
  a <- (1 / prior_sd^2) + (nt / yt_sd^2)
  m_tau[i] <- b / a

  sd_tau[i] <- 1 / a

  y_tau[i] <- rnorm(1, m_tau[i], sd_tau[i])
}

# Climatological distribution parameters (IC)
sd_clim <- .01 # Original = 0.01
mean_clim <- 0 # Original = 0

# Relative entropy - forecast horizon

D_t2 <- matrix(0, nrow = ft, ncol = 1)

for (i in 3:ft) {
  D_t2[i] <- 0.5 * ((log(sd_clim / sd_tau[i])) +
    (sd_tau[i] / sd_clim) + ((mean_clim - m_tau[i])^2) / sd_clim - 1)
}

plot(D_t2[3:40],
  type = "l", xlab = "Forecast horizon",
  ylab = "Relative Entropy"
)

# Mutual information - forecast horizon
MI_t2 <- matrix(0, nrow = ft, ncol = 1)

for (i in 3:ft) {
  MI_t2[i] <- -0.5 * (log(sd_tau[i]^2 / sd_clim^2))
}

plot(MI_t2[3:40],
  type = "l", xlab = "Forecast horizon",
  ylab = "Mutual Information "
)

S_t2 <- matrix(0, nrow = ft, ncol = 1)
Dis_t2 <- matrix(0, nrow = ft, ncol = 1)

for (i in 3:ft) {
  S_t2[i] <- ((mean_clim - m_tau[i])^2) / (2 * sd_clim)
  Dis_t2[i] <- 0.5 * ((log(sd_clim / sd_tau2[i])) +
    (sd_tau[i] / sd_clim) - 1)
}

S2D <- S_t2 / Dis_t2
lines(S_t2[3:40], col = "red")

# For unpredictable system - done

# Invariance to linear transformations - mathematical

# Poorly sampled time series

y_t2 <- y_t[750:1000]
