#############################################
## file for converting, cleaning and merging health data
## by: Alex&Torben
##################################################

# 0. Preparations
# 1. Classifications
# 2. Load csv sheets
# 3. Clean Health Data

####################################################
# 0. Preparations
###################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "readr", "plyr", "zoo","reshape", "spatstat")

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
# 1. Classifications
################################################


#################################################
# 2. Load csv Sheets 
#################################################
# If there are encoding issues, this one is really helpful: guess_encoding("health/ICD-10_F10-0.csv", n_max = 3)

F100 <- as.data.frame(read.csv2("health/ICD-10_F10-0.csv", skip = 15, 
                                header = F, fileEncoding ="ISO-8859-1", 
                                stringsAsFactors = FALSE))
F102 <- as.data.frame(read.csv2("health/ICD-10_F10-2.csv", skip = 15, 
                                header = F, fileEncoding = "ISO-8859-1", 
                                stringsAsFactors = FALSE))
K70 <- as.data.frame(read.csv2("health/ICD-10_K70.csv", skip = 15, 
                               header = F, fileEncoding = "ISO-8859-1", 
                               stringsAsFactors = FALSE))

###########################################
# 3. Clean Health Data
#############################################

# Delete last rows with stats sources etc.
F100 <- F100[-c(992:1001),]

# Delete empty last column
F100 <- F100[,-23]

# Delete leading and tailing whitespace
F100 <- as.data.frame(lapply(F100, function(x) trimws(x)), 
                      stringsAsFactors = FALSE)

# Recode empty cells
F100[F100 == ""] <- NA

# Filling the empty cells in YEAR and GENDER column
F100$V1 <- na.locf(F100$V1, na.rm = FALSE)
F100$V2 <- na.locf(F100$V2, na.rm = FALSE)

# Recode AGE and make it factor
F100$V3 <- mapvalues(F100$V3, 
                     c("Unter 1 Jahr", "1 Jahr bis unter 5 Jahre", 
                        "5 bis unter 10 Jahre", "10 bis unter 15 Jahre", 
                        "15 bis unter 20 Jahre", "20 bis unter 25 Jahre", 
                        "25 bis unter 30 Jahre", "30 bis unter 35 Jahre", 
                        "35 bis unter 40 Jahre", "40 bis unter 45 Jahre", 
                        "45 bis unter 50 Jahre", "50 bis unter 55 Jahre", 
                        "55 bis unter 60 Jahre", "60 bis unter 65 Jahre", 
                        "65 bis unter 70 Jahre", "70 bis unter 75 Jahre", 
                        "75 bis unter 80 Jahre", "80 bis unter 85 Jahre", 
                        "85 bis unter 90 Jahre", "90 bis unter 95 Jahre", 
                        "95 bis unter 100 Jahre", "100 Jahre und älter"), 
                     c("<1y", "1-4y", "5-9y", "10-14y", "15-19y", "20-24y", 
                       "25-29y", "30-34y", "35-39y", "40-44y", "45-49y", 
                       "50-54y", "55-59y", "60-64y", "65-69y", "70-74y", 
                       "75-79y", "80-84y", "85-89y", "90-94y", "95-99y", 
                       ">=100y"))
F100$AGE <- as.factor(F100$AGE)

# Recode STATE and make it factor
F100[1,] <- mapvalues(as.matrix(F100[1,]), 
                      c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", 
                        "Bremen", "Hamburg", "Hessen", 
                        "Mecklenburg-Vorpommern", "Niedersachsen", 
                        "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", 
                        "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", 
                        "Thüringen", "Früheres Bundesgebiet und Berlin-Ost", 
                        "Neue Länder ohne Berlin-Ost", 
                        "Ausland oder unbekannt"), 
                      c("DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH", 
                        "DE-HE", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SL", 
                        "DE-SN", "DE-ST", "DE-SH", "DE-TH", "DE-West", 
                        "DE-East", "Foreign_NA"))
F100$STATE <- as.factor(F100$STATE)

# Transpose STATES and rename columns
colnames(F100) <- c("YEAR", "GENDER", "AGE", F100[1,4:22])
F100 <- F100[-1,]
F100 <- melt(F100, id = 1:3, measured = 4:22)
colnames(F100) <- c("YEAR", "GENDER", "AGE", "STATE", "F100_CASES")

# Make F100 numeric
F100$F100_CASES <- as.numeric(as.character(F100$F100_CASES))
F100$F100_CASES <- round(F100$F100_CASES, 0)

# Recode GENDER and make it factor
F100$GENDER <- as.factor(F100$GENDER)
F100$GENDER <- mapvalues(F100$GENDER, from = c("Männlich", "Weiblich", "Unbekannt"), to = c("M", "F", "U"))

# Fill "NA" in F100_CASES with 0, as "no cases" was coded by an empty cell
F100$F100_CASES[is.na(F100$F100_CASES)] <- "0"


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
