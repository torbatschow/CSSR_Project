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
# 1. Model 3-5 regression
# 2. Robustness Check
# 3. Heat Map Germany    
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
packages <- c("dplyr", "stargazer", "ggplot2", "plm", "latticeExtra", "sp")
# can we add a short list so we have an overview for what we need the packages?
# dplyr for data manipulatio in data.frame, lfe for fixed effect estimation, 
# stargazer for regression tables, plm for panel data estimation
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
ICD <- c("F100_CASES", "F102_CASES", "K70_CASES")

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

# merge them to health
AGG <- rbind(AGG.A.G, AGG.A)
AGG <- rbind(AGG,AGG.G)
HEALTH <- rbind(HEALTH, AGG)

# Merge it
TOTAL <- merge(HEALTH, INDEP, by = c("STATE", "YEAR"))
TOTAL <- TOTAL[order(TOTAL$STATE, TOTAL$YEAR, TOTAL$GENDER, TOTAL$AGE),]

rm(HEALTH, INDEP, AGG, AGG.A, AGG.G, AGG.A.G, ICD, i)


################################################
# 2. Calculate Missing Variables
################################################

# Generate variables for whole Germany
Germany <- aggregate(F100_CASES ~ YEAR + GENDER + AGE, sum, data = TOTAL)
Germany <- cbind(STATE = "DE-DE", Germany)
Germany$F102_CASES <- aggregate(F102_CASES ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$K70_CASES <- aggregate(K70_CASES ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PD <- NA
Germany$UTOTAL <- aggregate(UTOTAL ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$UR.LF <- NA
Germany$UR <- NA
Germany$VAC <- aggregate(VAC ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PP <- aggregate(PP ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PP.EM <- aggregate(PP.EM ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PP.UE <- aggregate(PP.UE ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PP.EA <- aggregate(PP.EA ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$PP.EIA <- aggregate(PP.EIA ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$SA <- aggregate(SA ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$GDP <- aggregate(GDP ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$BTAX <- aggregate(BTAX ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$YUR <- NA
Germany$EDU_TOTAL <- aggregate(EDU_TOTAL ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$EDU_NO <- aggregate(EDU_NO ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$EDU_HS <- aggregate(EDU_HS ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$EDU_RS <- aggregate(EDU_RS ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$EDU_FH <- aggregate(EDU_FH ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany$EDU_AH <- aggregate(EDU_AH ~ YEAR + GENDER + AGE, sum, data = TOTAL)[,4]
Germany[,5:27] <- sapply(Germany[,5:27], as.numeric)
TOTAL <- rbind(TOTAL, Germany)
rm(Germany)

# GDP per capita (in tousand per capita)
# remark: just checked with data from 2014 and seems like our calculations are slightly higher.
#         This could be because of differing reference dates to the Dresden data.
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

# Model one ################################################################### 
# Simple Regression for each year 2000 to 2014 individually with aggregated data

# data selection for each model
for (i in 2000:2014)
{
  assign(paste("M1", i, sep = "."),
         filter(select(TOTAL, YEAR, STATE, AGE, GENDER, F100_p1000, GDP_P_C, UR.LF, PD),
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
         lm(F100_p1000 ~ GDP_P_C + UR.LF + PD, tmp))
  rm(tmp)
 }
mod1.list <- mget(ls(pattern = "mod1."))

# compare the models (robustness check)
# mod1.modellist <- as.character(c(2000:2014))
# stargazer::stargazer(mod1.list[1:5], 
#                     title = 'Regression Model 1 2000 - 2004',
#                     column.labels = mod1.modellist[1:5],
#                     model.numbers  = FALSE,
#                     dep.var.labels = "F10.0 Diagnoses per 1000 capita",
#                     digits = 2, type = 'text')
# stargazer::stargazer(mod1.list[6:10], 
#                     title = 'Regression Model 1 2005 - 2009',
#                     column.labels = mod1.modellist[6:10],
#                     model.numbers  = FALSE,
#                     dep.var.labels = "F10.0 Diagnoses per 1000 capita",
#                     digits = 2, type = 'text')
# stargazer::stargazer(mod1.list[11:15], 
#                     title = 'Regression Model 1 2010 - 2014',
#                     column.labels = mod1.modellist[11:15],
#                     model.numbers  = FALSE,
#                     dep.var.labels = "F10.0 Diagnoses per 1000 capita",
#                     digits = 2, type = 'text')

# Model two: Regression of differences ########################################

# first differencing the data (the lag operator might not function correctly)
M2 <- filter(TOTAL, GENDER == "all", AGE == "all") %>% 
            select(YEAR, STATE, AGE, GENDER, F100_p1000, F102_p1000, 
                   K70_p1000, GDP_P_C, UR.LF, PD)
M2 <- M2 %>% group_by(STATE) %>% mutate(F100.D = F100_p1000 - lag(F100_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(F102.D = F102_p1000 - lag(F102_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(K70.D = K70_p1000 - lag(K70_p1000))
M2 <- M2 %>% group_by(STATE) %>% mutate(GDP.D = GDP_P_C - lag(GDP_P_C))
M2 <- M2 %>% group_by(STATE) %>% mutate(UR.LF.D = UR.LF - lag(UR.LF))
M2 <- M2 %>% group_by(STATE) %>% mutate(PD.D = PD - lag(PD))

# dropping missing observations
M2 <- M2 %>% filter(YEAR > 2000)

# regression of equation 2
mod2.F100 <- lm(F100.D ~ GDP.D + UR.LF.D -1, M2)
mod2.F102 <- lm(F102.D ~ GDP.D + UR.LF.D -1, M2)
mod2.K70 <- lm(K70.D ~ GDP.D + UR.LF.D -1, M2)

# results
# stargazer::stargazer(mod2.F100, mod2.F102, mod2.K70,
#                     column.labels = c("F10.0", "F10.2", "K70"),
#                     covariate.labels = c("GDP Change", "Unemployment Change", "Beer Tax Change", "(Intercept)"),
#                     model.numbers  = FALSE,
#                     dep.var.labels = c("Change in F10.0","Change in F10.2","Change in K70"),
#                     title = 'Regression results for Model 2 with first differenced data',
#                     digits = 2, type = 'text', header = FALSE)

# Model three ###############################################################

# remark: what age group do we select here? 15-19, 20-24, aggregate both or all?
MDD <- TOTAL %>% filter(GENDER == "all", AGE == "all")
MDD$dBW <- as.numeric(MDD$STATE == "DE-BW")
MDD$dPOST <- as.numeric(MDD$YEAR >= 2010)
MDD$dBAN <- MDD$dBW * MDD$dPOST

mod3.16 <- lm(F100_p1000 ~ dBW + dPOST + dBW:dPOST, MDD)
mod3.13 <- lm(F100_p1000 ~ dBW + dPOST + dBW:dPOST, 
              filter(MDD, !(STATE %in% c("DE-HB", "DE-HH", "DE-BE"))))

#stargazer::stargazer(mod3.16, mod3.13, 
#                     title = 'Model 3 Simple Diff-in-Diff',
#                     digits = 2, type = 'text')

MDD.15 <- TOTAL %>% filter(GENDER == "all", AGE == "15-19y") %>% 
                    select(YEAR, STATE, AGE, GENDER, F100_p1000, GDP_P_C, YUR)

MDD.15$dBW <- as.numeric(MDD$STATE == "DE-BW")
MDD.15$dPOST <- as.numeric(MDD$YEAR >= 2010)
MDD.15$dBAN <- MDD$dBW * MDD$dPOST

mod3.age15.16 <- lm(F100_p1000 ~ dBW + dPOST + dBW:dPOST, MDD.15)
mod3.age15.13 <- lm(F100_p1000 ~ dBW + dPOST + dBW:dPOST, 
              filter(MDD.15, !(STATE %in% c("DE-HB", "DE-HH", "DE-BE"))))

stargazer::stargazer(mod3.age15.16, mod3.age15.13, 
                     title = 'Model 3 Simple Diff-in-Diff for 15-19 year old',
                     digits = 2, type = 'text')

# Model four ##################################################################

mod4 <- lm(F100_p1000 ~ dBW*dPOST +  GDP_P_C + YUR, MDD)

mod4.age15 <- lm(F100_p1000 ~ dBW*dPOST +  GDP_P_C + YUR, MDD.15)

stargazer::stargazer(mod4, mod4.age15,
                     title = 'Model 4 Simple Diff-in-Diff with controls',
                     digits = 2, type = 'text')

# Robustness check: different composition of control group
# idea 1: make regression to find trend for 2000-2010 -> 
# states with similar coefficient should be in the control group
# idea 2: exclude city states  HB, HH, BE (mod.13)

# Model five ##################################################################

# use plm package https://www.jstatsoft.org/article/view/v027i02/v27i02.pdf

mod5 <- plm(F100_p1000 ~ dBAN + YUR + GDP_P_C, MDD, effect = "twoways",
            index = c("STATE", "YEAR"))

mod5.age15.16 <- plm(F100_p1000 ~ dBAN + YUR + GDP_P_C, MDD.15, effect = "twoways",
            index = c("STATE", "YEAR"))

mod5.age15.13 <- plm(F100_p1000 ~ dBAN + YUR + GDP_P_C, 
                     filter(MDD.15, !(STATE %in% c("DE-HB", "DE-HH", "DE-BE"))),
                     effect = "twoways", index = c("STATE", "YEAR"))
stargazer::stargazer(mod5, mod5.age15.16, mod5.age15.13,
                     title = 'Model 5 Panel Diff-in-Diff',
                     digits = 2, type = 'text')

###############################################
# 4. Descriptive statistics
###############################################

# Interesting cases: 
# 1. common trend of F100 in BW and other states
# 2. general trend of F100, F102 and K70
# 3. trends of control variables
# 4. Germany heat map
#   http://stackoverflow.com/questions/34691798/heat-map-of-germany-using-spplot

# Diagnoses data trends #######################################################

# select diagnoses data for state comparison
# DS0 <- TOTAL %>% filter(GENDER == "all", AGE=="all") %>% 
#  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# F10.0 diagnoses time series for state comparison
# ggplot(data=DS0, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("F10.0 diagnoses per 1000 inhabliants") +
#  ggtitle("F10.0 diagnoses in German States 2000-2014")

# F10.2 diagnoses time series for state comparison
# ggplot(data=DS0, aes(x = YEAR, y = F102_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("F10.2 diagnoses per 1000 inhabliants") +
#  ggtitle("F10.2 diagnoses in German States 2000-2014")

# K70 diagnoses time series for state comparison
# ggplot(data=DS0, aes(x = YEAR, y = K70_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("K70 diagnoses per 1000 inhabliants") +
#  ggtitle("Alcohol-related liver diagnoses in German States 2000-2014")

# Plots ############

# PL1 <- TOTAL %>% filter(GENDER == "all", AGE!="all", STATE=="DE-DE")

# F10.0 cases per 1000 per age group
# ggplot(data=PL1, aes(x=AGE, y=F100_p1000)) +
#  geom_bar(stat="identity") +
#  xlab("Age Group") +
#  ylab("F10.0 cases per 1000 people") +
#  ggtitle("F10.0 cases in Germany per 1000 per age group") +
#  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

# K70 cases per 1000 per age group
# ggplot(data=PL1, aes(x=AGE, y=K70_p1000)) +
#   geom_bar(stat="identity") +
#  xlab("Age Group") +
#  ylab("K70 cases per 1000 people") +
#  ggtitle("K70 cases in Germany per 1000 per age group") +
#  theme(axis.text.x  = element_text(angle=90, vjust=0.5))



# control variable trend ################################################

 DS1 <- TOTAL %>% filter(GENDER == "all", AGE=="all", YEAR=="2014") %>% 
                 select(STATE, YEAR, GDP_P_C, UR.LF, YUR, BTAX_P_C, PP, PD)
# PP timeseries
 ggplot(data=DS1, aes(x = YEAR, y = PP, group = STATE, colour = STATE)) +   
  geom_line() +                              # line plot
  theme_bw() +                               # bw background
  xlab("Years") +                            
  ylab("POPULATION") +
  ggtitle("PP in German States 2000-2014")

 ggplot(data=DS1, aes(x = STATE, y = PD, colour = STATE)) +   
   geom_bar(stat = "identity") +
   theme_bw() +                               # bw background
   xlab("Years") +                            
   ylab("GDP per capita") +
   ggtitle("PP in German States 2014") 
 
# GDP timeseries
# ggplot(data=DS1, aes(x = YEAR, y = GDP_P_C, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("GDP per capita") +
#  ggtitle("GDP in German States 2000-2014")

# Unemployment rate 
# ggplot(data=DS1, aes(x = YEAR, y = UR.LF, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("Unemployment rate") +
#  ggtitle("Unemployment in German States 2000-2014")

# Youth unemployment rate
# ggplot(data=DS1, aes(x = YEAR, y = YUR, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("Youth unemployment rate") +
#  ggtitle("Youth unemployment in German States 2000-2014")

# Beer Tax
# REMARK: Beer TAX might be a bad proxy for beer consumption. 
# According to this statistic Bremen is the largest consumer, but it probably just hints at Becks
#ggplot(data=DS1, aes(x = YEAR, y = BTAX_P_C, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("Beer tax") +
#  ggtitle("Beer tax in German States 2000-2014")

# Common Trend assumption #####################################################

# select disgnoses data 15-19 and 20-24 year olds
# DS2 <- TOTAL %>% filter(GENDER == "all", AGE=="15-19y", STATE!="DE-DE") %>% 
#  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# DS3 <- TOTAL %>% filter(GENDER == "all", AGE=="20-24y") %>% 
#  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# DS4 <- TOTAL %>% filter(GENDER == "all", AGE=="40-44y") %>% 
#  select(STATE, YEAR, F100_p1000, F102_p1000, K70_p1000)

# Trend for 15-19 year olds
# interesting, something seems to have happend between 2012 and 2013
# ggplot(data=DS2, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("F10.0 diagnoses per 1000 inhabliants for 15-19 year olds") +
#  ggtitle("F10.0 diagnoses in German States 2000-2014 for 15-19 year olds")

# Trend for 20-24 year olds
# interesting, something seems to have happend between 2012 and 2013
# ggplot(data=DS3, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("F10.0 diagnoses per 1000 inhabliants for 20-24 year olds") +
#  ggtitle("F10.0 diagnoses in German States 2000-2014 for 20-24 year olds")

# Trend for 40-44 year olds
# interesting, something seems to have happend between 2012 and 2013
# ggplot(data=DS4, aes(x = YEAR, y = F100_p1000, group = STATE, colour = STATE)) +   
#  geom_line() +                              # line plot
#  theme_bw() +                               # bw background
#  xlab("Years") +                            
#  ylab("F10.0 diagnoses per 1000 inhabliants for 40-44 year olds") +
#  ggtitle("F10.0 diagnoses in German States 2000-2014 for 40-44 year olds")


# Map of Germany
# Get map file
# remark: The download doesn't work for me (the file cannot be opened after being downloaded, but if I it manually it works)
# remark reply: have you set your WD above? O:-)
if(class(try(readRDS("GermanyMap.rds")))=="try-error") {
  download.file("http://biogeo.ucdavis.edu/data/gadm2.8/rds/DEU_adm1.rds", destfile = "GermanyMap.rds")
}
map.Germany <- readRDS("GermanyMap.rds")
# Fix name issues
map.Germany$HASC_1 <- sub("\\.", "-", as.character(map.Germany$HASC_1))
map.Germany$HASC_1 <- sub("BR", "BB", as.character(map.Germany$HASC_1))
# Choose data
STATES.2000 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2000)
STATES.2005 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2005)
STATES.2006 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2006)
STATES.2007 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2007)
STATES.2010 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2010)
STATES.2014 <- TOTAL %>% filter(GENDER == "all", AGE == "all", STATE!="DE-DE", YEAR == 2014)
# Align order of states in map and data
map.Germany.2000 <- map.Germany[match(STATES.2000$STATE, map.Germany$HASC_1),]
map.Germany.2005 <- map.Germany[match(STATES.2005$STATE, map.Germany$HASC_1),]
map.Germany.2006 <- map.Germany[match(STATES.2006$STATE, map.Germany$HASC_1),]
map.Germany.2007 <- map.Germany[match(STATES.2007$STATE, map.Germany$HASC_1),]
map.Germany.2010 <- map.Germany[match(STATES.2010$STATE, map.Germany$HASC_1),]
map.Germany.2014 <- map.Germany[match(STATES.2014$STATE, map.Germany$HASC_1),]
# Choose variable to plot
map.Germany.2000.F100_p1000 <- map.Germany.2000
map.Germany.2000.F100_p1000$value <- STATES.2000$F100_p1000
map.Germany.2005.F100_p1000 <- map.Germany.2005
map.Germany.2005.F100_p1000$value <- STATES.2005$F100_p1000
map.Germany.2006.F100_p1000 <- map.Germany.2006
map.Germany.2006.F100_p1000$value <- STATES.2006$F100_p1000
map.Germany.2007.F100_p1000 <- map.Germany.2007
map.Germany.2007.F100_p1000$value <- STATES.2007$F100_p1000
map.Germany.2010.F100_p1000 <- map.Germany.2010
map.Germany.2010.F100_p1000$value <- STATES.2010$F100_p1000
map.Germany.2014.F100_p1000 <- map.Germany.2014
map.Germany.2014.F100_p1000$value <- STATES.2014$F100_p1000
# Choose plotting color pattern
# remark: I combined two color sequences and changed the order of one of them
colors <- c('#04152f','#052047','#08306b','#08519c','#2171b5','#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7', '#f7fbff',
            '#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d','#330007', '#1a0003')
# PLot it!
spplot(map.Germany.2014.F100_p1000, zcol = "value", col.regions = colors)


