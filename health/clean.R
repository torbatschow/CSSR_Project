#############################################
## file for converting, cleaning and merging health data
## by: Alex
##################################################

####################################################
# 0. Preparations
###################################################



# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/Pair_Assignment_2"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl")

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
# 2. load excel sheets 
#################################################
# legend: diag = diagnoses, g = gender, n = number, r = region/state
# reason for choice of package: http://stackoverflow.com/questions/7049272/importing-xlsx-file-into-r
# they are slightly different -> each one individually loaded

# ICD-10 4-digit gender&number by year
diag.g.n.2000 <- read_excel("health/5231301007015.xls", sheet = 5)

diag.g.n.2001 <- read_excel("health/5231301017015.xls", sheet = 5)

diag.g.n.2002 <- read_excel("health/5231301027015.xls", sheet = 5)
diag.g.n.2002[is.na(diag.g.n.2002)] <- 0

diag.g.n.2003 <- read_excel("health/5231301037015.xls", sheet = 5)

diag.g.n.2004 <- read_excel("health/5231301047015.xls", sheet = 5)

diag.g.n.2005 <- read_excel("health/5231301057015.xls", sheet = 6)

diag.g.n.2006 <- read_excel("health/5231301067015.xls", sheet = 6)
diag.g.n.2006[is.na(diag.g.n.2006)] <- 0

diag.g.n.2007 <- read_excel("health/5231301077015.xls", sheet = 6)
diag.g.n.2007[is.na(diag.g.n.2007)] <- 0

diag.g.n.2008 <- read_excel("health/5231301087015.xls", sheet = 6)
diag.g.n.2008[is.na(diag.g.n.2008)] <- 0

diag.g.n.2009 <- read_excel("health/5231301097015.xls", sheet = 6)
colnames(diag.g.n.2009) <- diag.g.n.2009[1,]
diag.g.n.2009 <- diag.g.n.2009[-1,]
diag.g.n.2009[, -(1:2)] <- sapply(diag.g.n.2009[, -(1:2)], as.numeric)
diag.g.n.2009[is.na(diag.g.n.2009)] <- 0

diag.g.n.2010 <- read_excel("health/5231301107015.xls", sheet = 5)
colnames(diag.g.n.2010) <- diag.g.n.2010[1,]
diag.g.n.2010 <- diag.g.n.2010[-1,]
diag.g.n.2010[, -(1:2)] <- sapply(diag.g.n.2010[, -(1:2)], as.numeric)
diag.g.n.2010[is.na(diag.g.n.2010)] <- 0

diag.g.n.2011 <- read_excel("health/5231301117015.xls", sheet = 5)
colnames(diag.g.n.2011) <- diag.g.n.2011[1,]
diag.g.n.2011 <- diag.g.n.2011[-1,]
diag.g.n.2011[, -(1:2)] <- sapply(diag.g.n.2011[, -(1:2)], as.numeric)
diag.g.n.2011[is.na(diag.g.n.2011)] <- 0

diag.g.n.2012 <- read_excel("health/5231301127015.xlsx", sheet = 5)
colnames(diag.g.n.2012) <- diag.g.n.2012[1,]
diag.g.n.2012 <- diag.g.n.2012[-1,]
diag.g.n.2012[is.na(diag.g.n.2012)] <- 0

diag.g.n.2013 <- read_excel("health/5231301137015.xlsx", sheet = 5)
colnames(diag.g.n.2013) <- diag.g.n.2013[1,]
diag.g.n.2013 <- diag.g.n.2013[-1,]
diag.g.n.2013[is.na(diag.g.n.2013)] <- 0

# ICD-10 3 digit by region&number
diag.r.n.2000 <- read_excel("health/5231301007015.xls", sheet = 9)

diag.r.n.2001 <- read_excel("health/5231301017015.xls", sheet = 9)

diag.r.n.2002 <- read_excel("health/5231301027015.xls", sheet = 9)
diag.r.n.2002[is.na(diag.r.n.2002)] <- 0

diag.r.n.2003 <- read_excel("health/5231301037015.xls", sheet = 9)

diag.r.n.2004 <- read_excel("health/5231301047015.xls", sheet = 9)

diag.r.n.2000 <- read_excel("health/5231301007015.xls", sheet = 9)

diag.r.n.2001 <- read_excel("health/5231301017015.xls", sheet = 9)

diag.r.n.2005 <- read_excel("health/5231301057015.xls", sheet = 10)

diag.r.n.2006 <- read_excel("health/5231301067015.xls", sheet = 10)
diag.r.n.2006[is.na(diag.r.n.2006)] <- 0

diag.r.n.2007 <- read_excel("health/5231301077015.xls", sheet = 10)
diag.r.n.2007[is.na(diag.r.n.2007)] <- 0

diag.r.n.2008 <- read_excel("health/5231301087015.xls", sheet = 10)
diag.r.n.2008[is.na(diag.r.n.2008)] <- 0

diag.r.n.2009 <- read_excel("health/5231301097015.xls", sheet = 10)
colnames(diag.r.n.2009) <- diag.r.n.2009[1,]
diag.r.n.2009 <- diag.r.n.2009[-1,]
diag.r.n.2009[, -(1:2)] <- sapply(diag.r.n.2009[, -(1:2)], as.numeric)
diag.r.n.2009[is.na(diag.r.n.2009)] <- 0

diag.r.n.2010 <- read_excel("health/5231301107015.xls", sheet = 9)
colnames(diag.r.n.2010) <- diag.r.n.2010[1,]
diag.r.n.2010 <- diag.r.n.2010[-1,]
diag.r.n.2010[, -(1:2)] <- sapply(diag.r.n.2010[, -(1:2)], as.numeric)
diag.r.n.2010[is.na(diag.r.n.2010)] <- 0

diag.r.n.2011 <- read_excel("health/5231301117015.xls", sheet = 9)
colnames(diag.r.n.2011) <- diag.r.n.2011[1,]
diag.r.n.2011 <- diag.r.n.2011[-1,]
diag.r.n.2011[, -(1:2)] <- sapply(diag.r.n.2011[, -(1:2)], as.numeric)
diag.r.n.2011[is.na(diag.r.n.2011)] <- 0

diag.r.n.2012 <- read_excel("health/5231301127015.xlsx", sheet = 9)
colnames(diag.r.n.2012) <- diag.r.n.2012[1,]
diag.r.n.2012 <- diag.r.n.2012[-1,]
diag.r.n.2012[is.na(diag.r.n.2012)] <- 0

diag.r.n.2013 <- read_excel("health/5231301137015.xlsx", sheet = 9)
colnames(diag.r.n.2013) <- diag.r.n.2013[1,]
diag.r.n.2013 <- diag.r.n.2013[-1,]
diag.r.n.2013[is.na(diag.r.n.2013)] <- 0


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
