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

##################################################
## CONTENT
##################################################
# 0. Preparations
# 1. Combine Data
# 2. Calculate Missing Variables
# 3. Calculate Models

####################################################
# 0. Preparations
###################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative 
          Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "readr", "plyr", "zoo", "reshape", "spatstat", 
              "lattice", "lfe")

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

# Merge it
TOTAL <- merge(HEALTH, INDEP, by = c("STATE", "YEAR"))
TOTAL <- TOTAL[order(TOTAL$STATE, TOTAL$YEAR, TOTAL$GENDER, TOTAL$AGE),]
rm(HEALTH, INDEP)


################################################
# 2. Calculate Missing Variables
################################################

# GDP per capita
TOTAL$GDP_P_C <- TOTAL$GDP/TOTAL$PP 

# Beer tax per capita
TOTAL$BTAX_P_C <- TOTAL$BTAX/TOTAL$PP

# F100 cases per 1000 people
TOTAL$F100_p1000 <- TOTAL$F100_CASES/TOTAL$PP


################################################
# 3. Calculate Models
################################################

#HEALTH$YEAR <- as.numeric(sub(".", "", as.character(HEALTH$YEAR), fixed = TRUE))

#xyplot(F102_CASES[STATE == "DE-East" & GENDER == "M"] ~ AGE, data = HEALTH)

# State FE
mod1 = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE, data = TOTAL)
summary(mod1)

# State and year FE
mod2 = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE + YEAR, data = TOTAL)
summary(mod2)
