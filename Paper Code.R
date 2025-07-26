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
# Last updated: April the 4th, 2025

library(readxl)
library(devtools)
library(openxlsx)
library(ConnectednessApproach)
library(zoo)
library(tseries)
library(moments)
library(nortest)
library(dplyr)
library(reshape2)
library(urca)




### Load the data

data <- read_excel("../Data/data.xlsx")
attach(data)
 

# Load the Z-Score data
 
zdata <- read_excel("../Data/zdata.xlsx")
attach(zdata)


###########################################################################
#### Figure1: Time series plots 
###########################################################################

# Create time series
DataO <- ts(data, start = c(2015, 9), frequency = 52)
# Set up a 3x2 plotting area
par(mfrow = c(3, 2),        # 3 rows, 2 columns
    mar = c(3, 3, 2, 1))    # Margins: bottom, left, top, right

# Plot each time series
plot.ts(DataO[, 2], col = "blue", xlab = "", ylab = "", main = "STOXX", lwd = 2)
plot.ts(DataO[, 3], col = "blue", xlab = "", ylab = "", main = "TRANSIRISK", lwd = 2)
plot.ts(DataO[, 4], col = "blue", xlab = "", ylab = "", main = "ESGPOSENT", lwd = 2)
plot.ts(DataO[, 5], col = "blue", xlab = "", ylab = "", main = "ESGNEGSENT", lwd = 2)
plot.ts(DataO[, 6], col = "blue", xlab = "", ylab = "", main = "ESGPOLSENT", lwd = 2)

###########################################################################
#### Table 1 : Descriptive statistics 
###########################################################################

# Extract variables
variables <- DataO[, 2:6] 

# Initialize an empty matrix to store the results
results <- matrix(NA, nrow = 10, ncol = ncol(variables))
colnames(results) <- colnames(variables)
rownames(results) <- c("Mean", "SD", "Max", "Min", "Skewness", "Kurtosis", "Jarque-Bera Statistic", "Jarque-Bera p-value", "Lilliefors Statistic", "Lilliefors p-value")

for (i in 1:ncol(variables)) {
  jb_test <- jarque.bera.test(variables[, i])
  lillie_test <- lillie.test(variables[, i])
  
  results["Mean", i] <- mean(variables[, i], na.rm = TRUE)
  results["SD", i] <- sd(variables[, i], na.rm = TRUE)
  results["Max", i] <- max(variables[, i], na.rm = TRUE)
  results["Min", i] <- min(variables[, i], na.rm = TRUE)
  results["Skewness", i] <- skewness(variables[, i], na.rm = TRUE)
  results["Kurtosis", i] <- kurtosis(variables[, i], na.rm = TRUE)
  results["Jarque-Bera Statistic", i] <- jb_test$statistic
  results["Jarque-Bera p-value", i] <- jb_test$p.value
  results["Lilliefors Statistic", i] <- lillie_test$statistic
  results["Lilliefors p-value", i] <- lillie_test$p.value
}

# Print the results table of descriptive statistics
print(results)

### BDS test:
# Run BDS test for the variable: STOXX
bds_STOXX <- bds.test(DataO[, 2], m = 5)
print(bds_STOXX)

# Run BDS test for the variable: TRANSIRISK
bds_TRANSIRISK <- bds.test(DataO[, 3], m = 5)
print(bds_TRANSIRISK)

# Run BDS test for for the variable: ESGPOSENT
bds_ESGPOSENT <- bds.test(DataO[, 4], m = 5)
print(bds_ESGPOSENT)

# Run BDS test for for the variable:ESGNEGSENT
bds_ESGNEGSENT <- bds.test(DataO[, 5], m = 5)
print(bds_ESGNEGSENT)

# Run BDS test for for the variable:ESGPLOSENT
bds_ESGPLOSENT <- bds.test(DataO[, 6], m = 5)
print(bds_ESGPLOSENT)

###########################################################################
# Stationaity tests (ADF, PP and KPSS) 
###########################################################################
DataZ <- ts(zdata, start = c(2015, 9), frequency = 52)
variablesZ <- DataZ[, 2:6] 

# Initialiser une liste pour stocker les résultats
adf_results <- list()
pp_results <- list()
kpss_results <- list()

# Boucle sur chaque variable
for (i in 1:ncol(variablesZ)) {
  var_name <- colnames(variablesZ)[i]
  cat("============== ", var_name, " ==============\n")
  
  # Série courante
  serie <- variablesZ[, i]
  
  # ADF Test (avec tendance et 4 retards comme dans EViews)
  adf <- ur.df(serie, type = "trend", lags = 16, selectlags = "AIC")
  cat("---- ADF Test ----\n")
  print(summary(adf))
  adf_results[[var_name]] <- adf
  
  # PP Test
  pp <- pp.test(serie, alternative = "stationary", type = "Z(t_alpha)")
  cat("---- PP Test ----\n")
  print(pp)
  pp_results[[var_name]] <- pp
  
  # KPSS Test (avec tendance)
  kpss <- ur.kpss(serie, type = "tau")
  cat("---- KPSS Test ----\n")
  print(summary(kpss))
  kpss_results[[var_name]] <- kpss
  
  cat("\n\n")
}


#%%%%%%%%%%%%%%%%%% CONNECTEDNESS APPROACH %%%%%%%%%%%%%%%%%%%%%%%%%%


zdata$DATE <- as.POSIXct(zdata$DATE, format = "%d/%m/%Y", tz = "UTC")

# STOXX vs TRANSIRISK vs ESGPOSENT

zoo_data1 <- zoo(zdata[, c(2, 3, 4)], order.by = zdata$DATE)

dca1 = ConnectednessApproach(zoo_data1,
                            nlag=1,
                            nfore=2, model="TVP-VAR",
                            window.size=5,
)

# STOXX vs TRANSIRISK vs ESGNEGSENT
zoo_data2 <- zoo(zdata[, c(2, 3, 5)], order.by = zdata$DATE)

dca2 = ConnectednessApproach(zoo_data2,
                             nlag=1,
                             nfore=2, model="TVP-VAR",
                             window.size=5,
)

# STOXX vs TRANSIRISK vs ESGPOLSENT
zoo_data3 <- zoo(zdata[, c(2, 3, 6)], order.by = zdata$DATE)

dca3 = ConnectednessApproach(zoo_data3,
                             nlag=1,
                             nfore=2, model="TVP-VAR",
                             window.size=5,
)

###########################################################################
## Table 2 : TVP-VAR based connectedness results
###########################################################################

dca1$TABLE
dca2$TABLE
dca3$TABLE

###########################################################################
### Figure 2 : Total connectedness index 
###########################################################################

        # STOXX vs TRANSIRISK vs ESGPOSENT

par(mar = c(3, 2, 4, 2) + 1) 
par(font.axis = 2)
PlotTCI(dca1, ylim = c(0, 70))
title('TCI: STOXX vs TRANSIRISK vs ESGPOSENT')

          # STOXX vs TRANSIRISK vs ESGNEGSENT

par(mar = c(3, 2, 4, 2) + 1) 
par(font.axis = 2)
PlotTCI(dca2, ylim = c(0, 70))
title('TCI: STOXX vs TRANSIRISK vs ESGNEGSENT')

       # STOXX vs TRANSIRISK vs ESGPOLSENT

par(mar = c(3, 2, 4, 2) + 1) 
par(font.axis = 2)
PlotTCI(dca3, ylim = c(0, 70))
title('TCI: STOXX vs TRANSIRISK vs ESGNEGSENT')



###########################################################################
### Figure 3 : Net Total Directional connectedness  
###########################################################################

                  # STOXX vs TRANSIRISK vs ESGPOSENT

PlotNET(dca1, ylim=c(-40,30))
NET1 = dca1$NET
             


###########################################################################
### Figure 4 : Net Total Directional connectedness  
###########################################################################

           # STOXX vs TRANSIRISK vs ESGNEGSENT

PlotNET(dca2, ylim=c(-40,40))
NET2 = dca2$NET


###########################################################################
### Figure 5 : Net Total Directional connectedness  
###########################################################################

            # STOXX vs TRANSIRISK vs ESGPOLSENT

PlotNET(dca3, ylim=c(-55,50))
NET3 = dca3$NET
            

###########################################################################
### Figure 6 : Net Pairwise Directional connectedness  
###########################################################################

           # STOXX vs TRANSIRISK vs ESGPOSENT

PlotNPDC(dca1, ylim=c(-35,25))



###########################################################################
### Figure 7 : Net Pairwise Directional connectedness  
###########################################################################

# STOXX vs TRANSIRISK vs ESGNEGSENT

PlotNPDC(dca2, ylim=c(-30,20))



###########################################################################
### Figure 8 : Net Pairwise Directional connectedness  
###########################################################################

# STOXX vs TRANSIRISK vs ESGPOLSENT

PlotNPDC(dca3, ylim=c(-35,25))


###########################################################################
### Figure 9 : plot Network NPDC  
###########################################################################

             # STOXX vs TRANSIRISK vs ESGPOSENT

PlotNetwork(dca1, method="NPDC")
mtext("STOXX vs TRANSIRISK vs ESGPOSENT", side = 3, line = 1, adj = 1, cex = 1.1, font = 1)


             # STOXX vs TRANSIRISK vs ESGNEGSENT

PlotNetwork(dca2, method="NPDC")
mtext("STOXX vs TRANSIRISK vs ESGNEGSENT", side = 3, line = 1, adj = 1, cex = 1.1, font = 1)

            # STOXX vs TRANSIRISK vs ESGPOLSENT

PlotNetwork(dca3, method="NPDC")
mtext("STOXX vs TRANSIRISK vs ESGPOLSENT", side = 3, line = 1, adj = 1, cex = 1.1, font = 1)

