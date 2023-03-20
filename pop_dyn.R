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


ggplot(data = df) +
  geom_point(aes(
    x = df[, "GrowthRate"], y = df[, "N"],
    col = df[, "PermutationEntropy"]
  )) +
  theme_bw() +
  scale_color_viridis_c()
