# simple model of logistic growth
dNt <- function(r, N) r * N * (1 - N)

# iterate growth through time
Nt <- function(r, N, t) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
  }
  N
}

t <- 100
r <- 0.1
# lets consider 4 different starting abundances (i.e., N(t=0) values)
Nt0 <- c(0.1, 0.5, 1.5, 2)


# set starting conditions and amount of time
t <- 1000
r <- seq(0.01, 3, .01)
Nt0 <- 0.5
# compute the population sizes across the times
e <- sapply(r, function(r) Nt(r, Nt0, t))

# only use 2nd half of times presuming those will be at equilibrium
thalf <- round(t / 2)
e <- e[thalf:t, ]
t <- nrow(e)
maxE <- max(as.vector(e))

# when r > 2, the population is unstable

# Looking at permutation entropy
pe <- matrix(0, nrow = ncol(e), ncol = 1)

for (i in seq_len(ncol(e))) {
  stable <- e[, i]
  od <- ordinal_pattern(stable, 3)
  pe[i, ] <- permu_entropy(od)
}

pal <- colorRampPalette(c("blue", "red"))
order <- findInterval(exp(pe), sort(pe))
ptsize <- 0.25
plot(rep(r[1], t), e[, 1],
  ylim = c(0, maxE), xlim = range(r),
  cex = ptsize, xlab = "Population growth rate (r)",
  ylab = "Equilibrium abundance"
)
for (i in seq_along(r)) {
  points(rep(r[i], t), e[, i], cex = ptsize, col = pal(nrow(df))[order])
}
abline(h = 1, col = "grey", lty = 2)

library(ggplot2)
r_m <- matrix(rep(r, each = t))
e_m <- matrix(e, nrow = nrow(r_m), ncol = 1)
pe_m <- matrix(rep(pe, each = t))

df <- as.data.frame(cbind(r_m, e_m, pe_m))
colnames(df) <- c("GrowthRate", "N", "PermutationEntropy")

# Calculate relative entropy
# Fit AR(1) to time series
# Climatological distribution is normal

# Set forecast horizon
ft <- 100
fh <- t:(t + ft)

res <- matrix(0, nrow = length(r))
rem <- matrix(0, nrow = length(r))

for (j in 201:300) {
  if (sum(e[, j]) == tmp) {
    rem[j] <- 1
    re_sum[j] <- 1
  } else {
    fit <- ar(e[, j], order.max = 1)
    pred <- matrix(0, nrow = ft, ncol = 1)
    pred[1] <- e[nrow(e), j]

    y_tau <- matrix(0, nrow = ft)
    m_tau <- matrix(0, nrow = ft, ncol = 1)
    sd_tau <- matrix(0, nrow = ft, ncol = 1)
    prior_sd <- 2
    prior_mean <- 0
    yt_mean <- mean(e[, j])
    yt_sd <- sd(e[, j])


    for (i in 2:ft) {
      if (fit$order == 0) {
        far <- 0
      }

      if (fit$order == 1) {
        far <- fit$ar
      }
      pred[i] <- pred[i - 1] * far + rnorm(1, 0, fit$var.pred)
      b <- ((1 / prior_sd^2) * prior_mean) + ((t / yt_sd^2) * pred[i])
      a <- (1 / prior_sd^2) + (t / yt_sd^2)
      m_tau[i] <- b / a

      sd_tau[i] <- 1 / a

      y_tau[i] <- rnorm(1, m_tau[i], sd_tau[i])
    }

    # Clim
    # Climatological distribution parameters (IC)
    sd_clim <- .01 # Original = 0.01
    mean_clim <- 0 # Original = 0

    # Relative entropy - forecast horizon

    D_t2 <- matrix(0, nrow = ft, ncol = 1)

    for (i in 2:ft) {
      D_t2[i] <- 0.5 * ((log(sd_clim / sd_tau[i])) +
        (sd_tau[i] / sd_clim) + ((mean_clim - m_tau[i])^2) / sd_clim - 1)
    }

    re_sum <- sum(D_t2)
    re_mean <- mean(D_t2[2:ft])

    rem[j] <- re_mean
    re_sum[j] <- re_sum
  }
}

rem[which(rem == 1)] <- 37
df$relative <- rep(rem, each = t)
# Plot

ggplot(data = df) +
  geom_point(aes(
    x = df[, "GrowthRate"], y = df[, "N"],
    col = df[, "PermutationEntropy"]
  )) +
  theme_bw() +
  scale_color_viridis_c() +
  ggtitle("Permutation Entropy")


ggplot(data = df) +
  geom_point(aes(
    x = df[, "GrowthRate"], y = df[, "N"],
    col = df[, "relative"]
  )) +
  theme_bw() +
  scale_color_viridis_c() +
  ggtitle("Relative Entropy")
