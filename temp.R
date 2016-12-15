# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# load data / models / graphs
source("main.R")

library(Zelig)

# Simulation of model 1 
Z1.2000 <- zelig(F100_p1000 ~ GDP_P_C + UR.LF + PD, cite = FALSE,
            data = M1.2000, model = "normal")

# Simulation of relationhip F10.0 diagnoses and GDP per capita
setZ.GDP.PP <- setx(Z1.2000, GDP_P_C = 15:46)
simZ.GDP.PP <- sim(Z1.2000, x = setZ.GDP.PP)
ci.plot(simZ.GDP.PP, ylab = "Conditional Exp. Change F10.0 Diagnoses per 1000", xlab = "")
title(xlab="Change of GDP", line=-1, cex.lab=1)

# Simulation of relationhip F10.0 diagnoses and unemployment rate
setZ.UR.LF <- setx(Z1.2000, UR.LF = 5:21)
simZ.UR.LF <- sim(Z1.2000, x = setZ.UR.LF)
ci.plot(simZ.UR.LF)

# Simulation of model 2 
Z2.F100 <- zelig(F100.D ~ GDP.D + UR.LF.D -1, cite = FALSE,
                 data = ungroup(M2), model = "normal")
  
# Simulation of relationhip F10.0 diagnoses and GDP per capita
setZ2.GDP.D <- setx(Z2.F100, GDP.D = -4:4)
simZ2.GDP.D <- sim(Z2.F100, x = setZ2.GDP.D)
ci.plot(simZ2.GDP.D)

# Simulation of relationhip F10.0 diagnoses and GDP per capita
setZ2.UR.LF.D <- setx(Z2.F100, UR.LF.D = -3:4)
simZ2.UR.LF.D <- sim(Z2.F100, x = setZ2.UR.LF.D)
ci.plot(simZ2.UR.LF.D, ylab = "Conditional Exp. Change F10.0 Diagnoses per 1000", xlab = "Change of unemployment")
