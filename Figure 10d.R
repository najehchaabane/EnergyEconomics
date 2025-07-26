# ============================================================
# Replication Code for the Paper:
# "Climate Transition Risks, ESG Sentiment, and Market Value:
#  Insights from the European Stock Market"
#
# Authors:
#   - Brahim Gaies
#   - Najeh Chaâbane (Code responsible)
#   - Opeoluwa Adeniyi Adeosun
#   - Jean-Michel Sahut
#
# Last updated: April the 21th, 2025


#Load the following auxiliary files : 
## quantile_coherency_replication_pack.R 
## install_required_packages.R

# set seed for RNG
set.seed(1234)

install.packages("readxl")
library(readxl)

# Load the data
y <- read_excel("../Data/zdata.xlsx", col_names = TRUE);


#####################################################
# Figure 10d: Quantile coherency STOXX vs ESGPOLSENT
#####################################################

Y1 <- y$STOXX
Y2 <- y$ESGPOLSENT

Y <- matrix(c(Y1, Y2), ncol = 2)
n <- dim(Y)[1]

## Compute Quantile cross-spectral densities on data

# chose quantile levels
quantile <- c(0.5, 0.05, 0.95)
sPG <- smoothedPG(Y, levels.1 = quantile,
                  weight = kernelWeight(W = W1, b = 0.5 * n^(-1/4)))

# Choose frequencies
cut = (0:123) / 124
cut = round(cut * 392) / 392

# Compute Quantile-Coherency and confidence intervals
Coh <- getCoherency(sPG, frequencies = 2 * pi * cut)

# NOTE: this takes few minutes
CI <- getPointwiseCIs(sPG, quantity = "coherency", frequencies = 2 * pi * cut)

dev.new()  # Clear any existing plots


par(mfrow = c(2, 2), mar = c(4, 2, 4.5, 0.5) + 0.1)

# Données communes
freq <- cut[1:(length(cut)/2)]
le <- (length(cut) / 2)
clr <- gray.colors(length(quantile) + 2, start = 0.1, end = 0.8)
clr <- c(clr[2], clr[1], clr[5])
d <- c(10, 20, 30)
a <- c(90, 90, 90)

### 1. Figure Re: tau1 = tau2
plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.5, 0.5), type = "l",
     xlab = expression(omega / 2*pi), ylab = "", main = "Quantile coherency (Re)")
for (i1 in 1:length(quantile)) {
  polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i1,2,i1])),
                                        Re(CI$up[1:le,1,i1,2,i1])),
          col = clr[i1], density = d[i1], angle = a[i1],
          lty = 3, lwd = 0.8)
  lines(x = freq, y = Re(Coh[1:le,1,i1,2,i1,1]),
        col = "black", lty = i1, lwd = 1.5)
}
axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")
legend("bottom", inset = .03, "center",
       c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.6,
       lwd = 1, lty = 1:length(quantile),
       horiz = TRUE, bg = "white")

### 2. Figure Re: tau1 ≠ tau2
plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.4, 0.4), type = "l",
     xlab = expression(omega / 2*pi), ylab = "", main = "Quantile coherency (Re)")
i1 <- 1; i2 <- 2; i3 <- 3
polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i2,2,i3])),
                                      Re(CI$up[1:le,1,i2,2,i3])),
        col = clr[i1], density = d[i1], angle = a[i1],
        lty = 3, lwd = 0.8)
lines(x = freq, y = Re(Coh[1:le,1,i2,2,i3,1]),
      col = "black", lty = i1, lwd = 1.5)
axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")

### 3. Figure Im: tau1 = tau2
plot(x = freq, xlim = c(0,0.5), ylim = c(-0.5, 0.4), type = "l",
     xlab = expression(omega / 2*pi), ylab = "", main = "Quantile coherency (Im)")
for (i1 in 1:length(quantile)) {
  polygon(x = c(rev(freq), freq), y = c(rev(Im(CI$lo[1:le,1,i1,2,i1])),
                                        Im(CI$up[1:le,1,i1,2,i1])),
          col = clr[i1], density = d[i1], angle = a[i1],
          lty = 3, lwd = 0.8)
  lines(x = freq, y = Im(Coh[1:le,1,i1,2,i1,1]),
        col = "black", lty = i1, lwd = 1.5)
}
axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")
legend("bottom", inset = .03, "center",
       c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.6,
       lwd = 1, lty = 1:length(quantile),
       horiz = TRUE, bg = "white")

### 4. Figure Im: tau1 ≠ tau2
plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.4, 0.4), type = "l",
     xlab = expression(omega / 2*pi), ylab = "", main = "Quantile coherency (Im)")
polygon(x = c(rev(freq), freq), y = c(rev(Im(CI$lo[1:le,1,i2,2,i3])),
                                      Im(CI$up[1:le,1,i2,2,i3])),
        col = clr[i1], density = d[i1], angle = a[i1],
        lty = 3, lwd = 0.8)
lines(x = freq, y = Im(Coh[1:le,1,i2,2,i3,1]),
      col = "black", lty = i1, lwd = 1.5)
axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")
################################################################################



