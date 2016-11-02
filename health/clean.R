#############################################
## file for converting, cleaning and merging health data
## by: Alex&Torben
##################################################

# 0. Preparations
# 1. Classifications
# 2. Load csv sheets
# 3. Merge to time series

####################################################
# 0. Preparations
###################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "readr")

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

# functions

# identify last characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

################################################
# 1. classifications
################################################

# Alcohol related diagnoses
# legend: alc = alcohol, icd10 = classification, 4 = four digit, 3 = three digit
# s = short
alc.icd10.4.s <- c("F100", "F101", "F102", "F103", "F104", "F105", "F106", "F107",
                   "F108", "F109", "T510", "T519")
# ex = extended
alc.icd10.4.ex  <- c("K700", "K701", "K702", "K703", "k704", "K709", "G312", 
                     "K292", "K860", "I426", "G405")

# 3 digit code like Marcus and Siedler (2014)
alc.icd10.3 <- c("F10", "T51")

#################################################
# 2. load csv sheets 
#################################################
# If there are encoding issues, this one is really helpful: guess_encoding("health/ICD-10_F10-0.csv", n_max = 3)

F100 <- read.csv2("health/ICD-10_F10-0.csv", skip = 15, header = F, sep= ";", fileEncoding ="ISO-8859-1")
F102 <- read.csv2("health/ICD-10_F10-2.csv", skip = 15, header = F, sep= ";", fileEncoding = "ISO-8859-1")
K70 <- read.csv2("health/ICD-10_K70.csv", skip = 15, header = F, sep= ";", fileEncoding = "ISO-8859-1")

###########################################
# 3. merge to time series
#############################################

#merged data frame with 4 digit ICD-10 code by gender and age
diag.g.n <- do.call(rbind,lapply(ls(pattern='diag.g.n..*'),
                     function(x) {
                       dat=get(x)
                       dat$year = substrRight(as.character(x), 4)
                       dat
                     }))

# merged data with 3 digit ICD-10 code by region
diag.r.n <- do.call(rbind,lapply(ls(pattern='diag.r.n..*'),
                                 function(x) {
                                   dat=get(x)
                                   dat$year = substrRight(as.character(x), 4)
                                   dat
                                 }))

# select alcohol related diagnoses
diag.alc.g.s <- diag.g.n[diag.g.n$`ICD-10-4` %in% alc.icd10.4.s,]
diag.alc.r <- diag.r.n[diag.r.n$`ICD-10-3` %in% alc.icd10.3,]

# export data in csv file
write.csv(diag.alc.g.s, file = "health/diag.alc.g.s.csv", row.names=FALSE)
write.csv(diag.alc.r, file = "health/diag.alc.r.csv", row.names=FALSE)

# for illustration - time line F10.0 diagnosis among men 2000-2013
plot(subset(diag.g.n$year, diag.g.n[, 1] == "F100" & diag.g.n[,2] == "m"),
     subset(diag.g.n$Insgesamt, diag.g.n[, 1] == "F100" & diag.g.n[,2] == "m"))
