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
#    Done 
# 2. Model 3-5 regression
# 3. Descriptive statistics
#    Basic line plots are done
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
packages <- c("dplyr", "lfe", "stargazer", "ggplot2")
# can we add a short list so we have an overview for what we need the packages?
# dplyr for data manipulatio in data.frame, lfe for fixed effect estimation, 
# stargazer for regression tables
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

rm(HEALTH, INDEP, AGG, AGG.A, AGG.G, AGG.A.G)


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
TOTAL$F102_p1000 <- TOTAL$F102_CASES/TOTAL$PP
TOTAL$K70_p1000 <- TOTAL$K70_CASES/TOTAL$PP 


################################################
# 3. Calculate Models
################################################

# Model one: Simple Regression for each year 2000 to 2014 individually with aggregated data

# data selection for each model
for (i in 2000:2014)
{
  assign(paste("M1", i, sep = "."),
         filter(select(TOTAL, YEAR, STATE, AGE, GENDER, F100_p1000, GDP_P_C, UR.LF, BTAX_P_C, PD),
                AGE == "all", GENDER == "all", YEAR == i))
  }

# list of models
M1.list <- mget(ls(pattern = "M1."))

# regression of each model
for (i in 1:15)
 {
  tmp <- as.data.frame(M1.list[i])
  colnames(tmp) <- substring(colnames(tmp),9)
  assign(paste("mod1", substring(names(M1.list[i]),4), sep = "."), 
         lm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD, tmp))
  rm(tmp)
 }
mod1.list <- mget(ls(pattern = "mod1."))

#compare the models (robustness check)
stargazer::stargazer(mod1.list[1:5], 
                     title = 'Regression Model 1 2000 - 2004',
                     digits = 2, type = 'text')
stargazer::stargazer(mod1.list[6:10], 
                     title = 'Regression Model 1 2005 - 2009',
                     digits = 2, type = 'text')
stargazer::stargazer(mod1.list[11:15], 
                     title = 'Regression Model 1 2010 - 2014',
                     digits = 2, type = 'text')

# Model two: Regression of differences

# first differencing the data (the lag operator might not function correctly)
M2 <- filter(TOTAL, GENDER == "all", AGE == "all")
M2 <- M2 %>% group_by(STATE) %>% mutate(F100.D = F100_p1000 - lag(F100_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(F102.D = F102_p1000 - lag(F102_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(K70.D = K70_p1000 - lag(K70_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(GDP.D = GDP_P_C - lag(GDP_P_C))
M2 <- M2 %>% group_by(STATE) %>% mutate(UR.LF.D = UR.LF - lag(UR.LF))
M2 <- M2 %>% group_by(STATE) %>% mutate(BTAX_P_C.D = BTAX_P_C - lag(BTAX_P_C))
M2 <- M2 %>% group_by(STATE) %>% mutate(PD.D = PD - lag(PD))

# dropping missing observations
M2 <- M2 %>% filter(YEAR > 2000)

# regression of equation 2
mod2.F100 <- lm(F100.D ~ GDP.D + UR.LF.D + BTAX_P_C.D -1, M2)
mod2.F102 <- lm(F102.D ~ GDP.D + UR.LF.D + BTAX_P_C.D -1, M2)
mod2.K70 <- lm(K70.D ~ GDP.D + UR.LF.D + BTAX_P_C.D -1, M2)


#xyplot(F102_CASES[STATE == "DE-East" & GENDER == "M"] ~ AGE, data = HEALTH)

# remark: we treat each age group as a different observation here. Is that feasible?
# State FE
modA = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE, data = TOTAL)
summary(modA)

# State and year FE
modB = felm(F100_p1000 ~ GDP_P_C + UR.LF + BTAX_P_C + PD | STATE + YEAR, data = TOTAL)
summary(modB)


###############################################
# 4. Descriptive statistics
###############################################

# Interesting cases: 
# 1. common trend of F100 in BW and other states
# 2. general trend of F100, F102 and K70
# 3. trends of control variables

# Diagnoses data trends #######################################################

# select disgnoses data
DS0 <- TOTAL %>% filter(GENDER == "all", AGE=="all") %>% 
  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# F10.0 diagnoses time series
ggplot(data=DS0, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("F10.0 diagnoses per 1000 inhabliants") +
  ggtitle("F10.0 diagnoses in German States 2000-2014")

# F10.2 diagnoses time series
ggplot(data=DS0, aes(x = YEAR, y = F102_p1000, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("F10.2 diagnoses per 1000 inhabliants") +
  ggtitle("F10.2 diagnoses in German States 2000-2014")

# K70 diagnoses time series
ggplot(data=DS0, aes(x = YEAR, y = K70_p1000, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("K70 diagnoses per 1000 inhabliants") +
  ggtitle("Alcohol-related liver diagnoses in German States 2000-2014")

# control variable trend ################################################

DS1 <- TOTAL %>% filter(GENDER == "all", AGE=="all") %>% 
                 select(STATE, YEAR, GDP_P_C, UR.LF, YUR, BTAX_P_C)
# GDP timeseries
ggplot(data=DS1, aes(x = YEAR, y = GDP_P_C, group = STATE, colour = STATE)) +   
       geom_line() +                              # line plot
       theme_bw() +                               # bw background
       xlab("Years") +                            
       ylab("GDP per capita") +
       ggtitle("GDP in German States 2000-2014")

# Unemployment rate 
ggplot(data=DS1, aes(x = YEAR, y = UR.LF, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("Unemployment rate") +
  ggtitle("Unemployment in German States 2000-2014")

# Youth unemployment rate
# REMARK: Something is wrong with the youth unemployment data set!!!!
ggplot(data=DS1, aes(x = YEAR, y = YUR, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("Youth unemployment rate") +
  ggtitle("Youth unemployment in German States 2000-2014")

# Beer Tax
# REMARK: Beer TAX might be a bad proxy for beer consumption. 
# According to this statistic Bremen is the largest consumer, but it probably just hints at Becks
ggplot(data=DS1, aes(x = YEAR, y = BTAX_P_C, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("Unemployment rate") +
  ggtitle("Unemployment in German States 2000-2014")

# Common Trend assumption #####################################################

# select disgnoses data 15-19 and 20-24 year olds
DS2 <- TOTAL %>% filter(GENDER == "all", AGE=="15-19y") %>% 
  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

DS3 <- TOTAL %>% filter(GENDER == "all", AGE=="20-24y") %>% 
  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# Trend for 15-19 year olds
# interesting, something seems to have happend between 2012 and 2013
ggplot(data=DS2, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("F10.0 diagnoses per 1000 inhabliants for 15-19 year olds") +
  ggtitle("F10.0 diagnoses in German States 2000-2014 for 15-19 year olds")

# Trend for 20-24 year olds
# interesting, something seems to have happend between 2012 and 2013
ggplot(data=DS3, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("F10.0 diagnoses per 1000 inhabliants for 20-24 year olds") +
  ggtitle("F10.0 diagnoses in German States 2000-2014 for 20-24 year olds")