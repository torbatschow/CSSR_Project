# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# load data / models / graphs
source("main.R")

library(Zelig)

Z1 <- zelig(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD, cite = FALSE,
            data = M1.2000, model = "normal")
setZ1 <- setx(Z1, GDP_P_C = 15:46)
simZ1 <- sim(Z1, x = setZ1)
ci.plot(simZ1)