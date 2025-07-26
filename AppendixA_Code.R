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
####     Appendix A: TVP-VAR approach & GARCH model                    ####
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
vdata <- read_excel("../Data/V_data.xlsx")
attach(vdata)

#%%%%%%%%%%%%%%%%%% CONNECTEDNESS APPROACH %%%%%%%%%%%%%%%%%%%%%%%%%%

vdata$Date <- as.POSIXct(vdata$Date, format = "%d/%m/%Y", tz = "UTC")

######## STOXX-V vs TRANSIRISK vs ESGPOSENT ########

zoo_data_v1 <- zoo(vdata[, c(2, 3, 4)], order.by = vdata$Date)

dcav1 = ConnectednessApproach(zoo_data_v1,
                              nlag=1,
                              nfore=2, model="TVP-VAR",
                              window.size=5,
)

#########   STOXX-V vs TRANSIRISK vs ESGNEGSENT  ########

zoo_data_v2 <- zoo(vdata[, c(2, 3, 5)], order.by = vdata$Date)

dcav2 = ConnectednessApproach(zoo_data_v2,
                              nlag=1,
                              nfore=2, model="TVP-VAR",
                              window.size=5,
)

#########   STOXX-V vs TRANSIRISK vs ESGPOLSENT  ########

zoo_data_v3 <- zoo(vdata[, c(2, 3, 6)], order.by = vdata$Date)

dcav3 = ConnectednessApproach(zoo_data_v3,
                              nlag=1,
                              nfore=2, model="TVP-VAR",
                              window.size=5,
)

###########################################################################
## Table A1 : TVP-VAR based connectedness applied to variables volatilities
###########################################################################

dcav1$TABLE
dcav2$TABLE
dcav3$TABLE


###########################################################################
### Figure A1 : TCI over time between variable volatilities
###########################################################################

######### STOXX-V vs TRANSIRISK vs ESGPOSENT ########
par(mar=c(3,2,2,2)+0.1)
par(font.axis = 2)
PlotTCI(dcav1, ylim=c(0,50))
mtext("TCI : STOXX-V vs TRANSIRISK vs ESGPOSENT", side = 3, line = 1, adj = 0.9, cex = 1.1, font = 2)

######### STOXX-V vs TRANSIRISK vs ESGNEGSENT ########
par(mar=c(3,2,2,2)+0.1)
PlotTCI(dcav2, ylim=c(0,50))
mtext("TCI : STOXX-V vs TRANSIRISK vs ESGNEGSENT", side = 3, line = 1, adj = 0.9, cex = 1.1, font = 2)

######### STOXX-V vs TRANSIRISK vs ESGPOLSENT ########
par(mar=c(3,2,2,2)+0.1)
PlotTCI(dcav3, ylim=c(0,60))
mtext("TCI : STOXX-V vs TRANSIRISK vs ESGPOLSENT", side = 3, line = 1, adj = 0.9, cex = 1.1, font = 2)

###########################################################################
### Figure A2 : Net Total Directional connectedness  
#               STOXX-V vs TRANSIRISK vs ESGPOSENT
###########################################################################


PlotNET(dcav1, ylim=c(-35,35))


###########################################################################
### Figure A3 : Net Total Directional connectedness  
# STOXX-V vs TRANSIRISK vs ESGNEGSENT
###########################################################################

PlotNET(dcav2, ylim=c(-35,35))

###########################################################################
### Figure A4 : Net Total Directional connectedness 
# STOXX-V vs TRANSIRISK vs ESGPOLSENT
###########################################################################


PlotNET(dcav3, ylim=c(-35,35))





