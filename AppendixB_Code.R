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

###########################################################################
####     Appendix B: TVP-VAR approach applied to US context            ####
###########################################################################


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




### Load the data
data <- read_excel("../Data/US_data.xlsx")
attach(data)

DataO <- ts(data, start = c(2003, 2), frequency = 12)

###########################################################################
#### Table B1 : Descriptive statistics 
###########################################################################

# Extract variables
variables <- DataO[, 2:4] 

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

# Run BDS test for the variable: SP500
bds_SP500 <- bds.test(DataO[, 2], m = 5)
print(bds_SP500)

# Run BDS test for the variable: ESGUI
bds_ESGUI <- bds.test(DataO[, 3], m = 5)
print(bds_ESGUI)

# Run BDS test for for the variable: USclimaticRisk
bds_USclimaticRisk <- bds.test(DataO[, 4], m = 5)
print(bds_USclimaticRisk)

###########################################################################
# Stationaity tests (ADF, PP and KPSS) 
###########################################################################

# Initialiser une liste pour stocker les résultats
adf_results <- list()
pp_results <- list()
kpss_results <- list()

# Boucle sur chaque variable
for (i in 1:ncol(variables)) {
  var_name <- colnames(variables)[i]
  cat("============== ", var_name, " ==============\n")
  
  # Série courante
  serie <- variables[, i]
  
  # ADF Test (avec tendance et 4 retards comme dans EViews)
  adf <- ur.df(serie, type = "drift", lags = 16, selectlags = "AIC")
  cat("---- ADF Test ----\n")
  print(summary(adf))
  adf_results[[var_name]] <- adf
  
  # PP Test
  pp <- pp.test(serie, alternative = "stationary", type = "Z(t_alpha)")
    cat("---- PP Test ----\n")
  print(pp)
  pp_results[[var_name]] <- pp
  
  # KPSS Test (avec tendance)
  kpss <- ur.kpss(serie, type = "mu")
  cat("---- KPSS Test ----\n")
  print(summary(kpss))
  kpss_results[[var_name]] <- kpss
  
  cat("\n\n")
}


###########################################################################
                  #### Connectedness Approach ####
###########################################################################

zoo_data <- zoo(data[, c(2, 4, 3)], order.by = data$date)

dca= ConnectednessApproach(zoo_data,
                             nlag=1,
                             nfore=15, model="TVP-VAR",
                             window.size=5,
)


###########################################################################
## Table B2 : TVP-VAR based connectedness results
###########################################################################
dca$TABLE

###########################################################################
### Figure B2 : Total connectedness index 
###########################################################################

par(mar=c(3,2,2,2)+0.1)
par(font.axis = 2)
PlotTCI(dca, ylim=c(0,68))
mtext("TCI : SP500 vs USclimaticRisk vs ESGUI", side = 3, line = 1, adj = 0.9, cex = 1.1, font = 2)

###########################################################################
### Figure B2 : Net Total Directional connectedness  
###########################################################################
PlotNET(dca, ylim=c(-100,130))


###########################################################################
### Figure B3 : Net Total Directional connectedness  
###########################################################################


PlotNPDC(dca, ylim=c(-80,40))

