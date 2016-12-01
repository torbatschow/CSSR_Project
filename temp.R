# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# load data / models / graphs
source("main.R")

library(Zelig)

Z1.2000 <- zelig(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD, cite = FALSE,
            data = M1.2000, model = "normal")
setZ.GDP.PP <- setx(Z1.2000, GDP_P_C = 15:46)
simZ.GDP.PP <- sim(Z1.2000, x = setZ.GDP.PP)
ci.plot(simZ.GDP.PP)


setZ.UR.LF <- setx(Z1.2000, UR.LF = 15:46)