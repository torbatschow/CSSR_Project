##################################################
## File for combining all data and calculating the models
## by: Alex&Torben
##################################################

##################################################
## REMARKS
##################################################
# At the end of this script, we have a complete dataframe named TOTAL containing all
# dependend and independend variables, including additionally calculated ones:
# ******TO BE COMPLETED******
# ToDo:
# 1. Aggregate total for age and gender
#
#
#
#
#




##################################################
## CONTENT
##################################################
# 0. Preparations
# 1. Combine Data
# 2. Calculate Missing Variables
# 3. Calculate Models
# 4. Descriptive Statistics

####################################################
# 0. Preparations
###################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("dplyr", "lfe")
# can we add a short list so we have an overview for what we need the packages?
# dplyr for data manipulatio in data.frame, lfe for fixed effect estimation
# removed: , "spatstat", "lattice",

# install packages if not installed before
for (p in packages) {
  if (p %in% installed.packages()[,1]) {
    require(p, character.only=T)
  }
  else {
    install.packages(p, repos="http://cran.rstudio.com", dependencies = TRUE)
    require(p, character.only=T)
  }
}
rm(p, packages)


################################################
# 1. Combine Data
################################################

# Get health data
source("health/getHealth.R", encoding = "utf-8")

# Get control data
source("control/getControl.R", encoding = "utf-8")

# Cases list
# ICD <- c("F100_CASES", "F102_CASES", "K70_CASES")
# HEALTH.A <- data.frame(NA,NA,NA,NA)
# colnames(HEALTH.A) <- c("STATE", "YEAR", "GENDER", "AGE")
# for (k in ICD)
# {
#  AGG.A.G <- aggregate(as.numeric(HEALTH$ICD[k]), by = list(HEALTH$YEAR, HEALTH$STATE),
#                       FUN = sum)
# }
ICD <- c("F100_CASES", "F102_CASES", "K70_CASES")
#HEALTH.A <- data.frame(NA, NA, NA, NA)
#colnames(HEALTH.A) <- c("STATE","YEAR", "GENDER", "AGE")

# sum up all ages, gender (long way)
AGG.A.G <- aggregate(as.numeric(HEALTH[,5]), by = list(HEALTH$STATE, HEALTH$YEAR),
                     FUN = sum)
for (i in 6:7)
 {
   AGG.A.G[,ICD[i-4]] <- aggregate(as.numeric(HEALTH[,i]), by = list(HEALTH$STATE, HEALTH$YEAR),
                                FUN = sum)[[3]]
 }

colnames(AGG.A.G) <- c("STATE","YEAR", ICD[1:3])
AGG.A.G$AGE <- as.factor("all")
AGG.A.G$GENDER <- as.factor("all")

# sum up all gender for each age
AGG.G <- aggregate(as.numeric(HEALTH[,5]), by = list(HEALTH$STATE, HEALTH$YEAR, HEALTH$AGE),
                   FUN = sum)
for (i in 6:7)
{
  AGG.G[,ICD[i-4]] <- aggregate(as.numeric(HEALTH[,i]), by = list(HEALTH$STATE, HEALTH$YEAR, HEALTH$AGE),
                                  FUN = sum)[[4]]
}

colnames(AGG.G) <- c("STATE","YEAR", "AGE", ICD[1:3])
AGG.G$GENDER <- as.factor("all")

# sum up all ages for each gender
AGG.A <- aggregate(as.numeric(HEALTH[,5]), by = list(HEALTH$STATE, HEALTH$YEAR, HEALTH$GENDER),
                   FUN = sum)
for (i in 6:7)
{
  AGG.A[,ICD[i-4]] <- aggregate(as.numeric(HEALTH[,i]), by = list(HEALTH$STATE, HEALTH$YEAR, HEALTH$GENDER),
                                FUN = sum)[[4]]
}

colnames(AGG.A) <- c("STATE","YEAR", "GENDER", ICD[1:3])
AGG.A$AGE <- as.factor("all")

#merge them to health
AGG <- rbind(AGG.A.G, AGG.A)
AGG <- rbind(AGG,AGG.G)
HEALTH <- rbind(HEALTH, AGG)

# Merge it
TOTAL <- merge(HEALTH, INDEP, by = c("STATE", "YEAR"))
TOTAL <- TOTAL[order(TOTAL$STATE, TOTAL$YEAR, TOTAL$GENDER, TOTAL$AGE),]

# rm(HEALTH, INDEP)


################################################
# 2. Calculate Missing Variables
################################################

# GDP per capita (in tousand per capita)
# remark: just checked with data from 2014 and seems like our calculations are slightly higher
# http://statistik-dresden.de/archives/11841
TOTAL$GDP_P_C <- TOTAL$GDP/TOTAL$PP 

# GDP per economic active
# https://www.statistik.sachsen-anhalt.de/apps/StrukturKompass/indikator/zeitreihe/124
# TOTAL$GDP_P_EA <- TOTAL$GDP/TOTAL$PP.EA

# Beer tax per capita
TOTAL$BTAX_P_C <- TOTAL$BTAX/TOTAL$PP

# F100 cases per 1000 people
TOTAL$F100_p1000 <- TOTAL$F100_CASES/TOTAL$PP
TOTAL$F100_p1000 <- TOTAL$F102_CASES/TOTAL$PP
TOTAL$K70_p1000 <- TOTAL$K70_CASES/TOTAL$PP 


################################################
# 3. Calculate Models
################################################

#HEALTH$YEAR <- as.numeric(sub(".", "", as.character(HEALTH$YEAR), fixed = TRUE))

#xyplot(F102_CASES[STATE == "DE-East" & GENDER == "M"] ~ AGE, data = HEALTH)

# remark: we treat each age group as a different observation here. Is that feasible?
# State FE
mod1 = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE, data = TOTAL)
summary(mod1)

# State and year FE
mod2 = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE + YEAR, data = TOTAL)
summary(mod2)


###############################################
# 4. Descriptive statistics
###############################################

# X axis
YEARS <- c(2000:2014)
PP.Y <- filter(TOTAL, AGE == "1-4y" & GENDER == "M")