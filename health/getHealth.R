#############################################
## file for converting, cleaning and merging health data
## by: Alex&Torben
##################################################

##################################################
## REMARKS
##################################################
# At the end of this script, we have a dataframe named HEALTH containing all
# cases of F10.0, F10.2, and K70 for:
# YEAR (2000 - 2014)
# GENDER (M = male, F = female, U = unknown)
# AGE (5-year age groups)
# STATE (Patients residents in 16 German States, plus Western Germany, 
#        Eastern Germany, and Foreign)

##################################################
## CONTENT
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
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative 
          Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "readr", "plyr", "zoo","reshape")
# remark: can we add a short list so we have an overview for what we need the packages?
# readxl = read excel files, readr for read_csv, reshape for melt function, 
# plyr for mapvalues function, zoo for na.locf function 
# where in list, but not needed for this script or: "spatstat","lattice", "lfe"

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

# Generate list of states
statelist_name <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", 
                    "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", 
                    "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", 
                    "Saarland", "Sachsen", "Sachsen-Anhalt", 
                    "Schleswig-Holstein", "Thüringen")

statelist_code <- c("DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH", 
                    "DE-HE", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SL",
                    "DE-SN", "DE-ST", "DE-SH", "DE-TH")

# Generate list of age groups
agelist_name <- c("Unter 1 Jahr", "1 Jahr bis unter 5 Jahre", 
                  "5 bis unter 10 Jahre", "10 bis unter 15 Jahre", 
                  "15 bis unter 20 Jahre", "20 bis unter 25 Jahre", 
                  "25 bis unter 30 Jahre", "30 bis unter 35 Jahre", 
                  "35 bis unter 40 Jahre", "40 bis unter 45 Jahre", 
                  "45 bis unter 50 Jahre", "50 bis unter 55 Jahre", 
                  "55 bis unter 60 Jahre", "60 bis unter 65 Jahre", 
                  "65 bis unter 70 Jahre", "70 bis unter 75 Jahre", 
                  "75 bis unter 80 Jahre", "80 bis unter 85 Jahre", 
                  "85 bis unter 90 Jahre", "90 bis unter 95 Jahre", 
                  "95 bis unter 100 Jahre", "100 Jahre und älter")

agelist_code <- c("<1y", "1-4y", "5-9y", "10-14y", "15-19y", "20-24y", 
                  "25-29y", "30-34y", "35-39y", "40-44y", "45-49y", "50-54y",
                  "55-59y", "60-64y", "65-69y", "70-74y", "75-79y", "80-84y",
                  "85-89y", "90-94y", "95-99y", "100y +")

################################################
# 1. Classifications
################################################


#################################################
# 2. Load csv Sheets 
#################################################
# If there are encoding issues, this one is really helpful: 
# guess_encoding("health/ICD-10_F10-0.csv", n_max = 3)

F100 <- as.data.frame(read.csv2("health/ICD-10_F10-0.csv", skip = 15, 
                                header = F, fileEncoding ="ISO-8859-1", 
                                stringsAsFactors = FALSE, dec = ","))
F102 <- as.data.frame(read.csv2("health/ICD-10_F10-2.csv", skip = 15, 
                                header = F, fileEncoding = "ISO-8859-1", 
                                stringsAsFactors = FALSE, dec = ","))
K70 <- as.data.frame(read.csv2("health/ICD-10_K70.csv", skip = 15, 
                               header = F, fileEncoding = "ISO-8859-1", 
                               stringsAsFactors = FALSE, dec = ","))

###########################################
# 3. Clean Health Data
###########################################

###########################################
# 3.1 ICD-10 Code F10.0 
###########################################

# Delete last rows with stats sources etc.
F100 <- F100[-c(992:1001),]

# Delete empty last column
F100 <- F100[,-23]

# Delete leading and tailing whitespace
F100 <- as.data.frame(lapply(F100, function(x) trimws(x)), 
                      stringsAsFactors = FALSE)

# Recode empty cells
F100[F100 == ""] <- NA

# Rename columns
colnames(F100) <- c("YEAR", "GENDER", "AGE", F100[1,4:22])
F100 <- F100[-1,]

# Transpose STATES and rename columns
F100 <- melt(F100, id = 1:3, measured = 4:22)
colnames(F100) <- c("YEAR", "GENDER", "AGE", "STATE", "F100_CASES")

# Filling the empty cells in YEAR and GENDER column
F100$YEAR <- na.locf(F100$YEAR, na.rm = FALSE)
F100$GENDER <- na.locf(F100$GENDER, na.rm = FALSE)

# Recode GENDER and make it factor
F100$GENDER <- as.factor(F100$GENDER)
F100$GENDER <- mapvalues(F100$GENDER, from = c("Männlich", "Weiblich", 
                                               "Unbekannt"), to = c("M", "F", 
                                                                    "U"))
F100$GENDER <- factor(F100$GENDER, levels = c("M", "F", "U"))

# Recode AGE and make it factor
F100$AGE <- mapvalues(F100$AGE, agelist_name, agelist_code)
F100$AGE <- as.factor(F100$AGE)
F100$AGE <- factor(F100$AGE, levels = agelist_code)

# Recode STATE and make it factor
F100$STATE <- mapvalues(as.matrix(F100$STATE), statelist_name, statelist_code)
F100$STATE <- as.factor(F100$STATE)
F100$STATE <- factor(F100$STATE, levels = statelist_code)

# Make F100_CASES numeric
F100$F100_CASES <- as.numeric(sub(".", "", as.character(F100$F100_CASES), 
                                  fixed = TRUE))

# Fill "NA" in F100_CASES with 0, as "no cases" was coded by an empty cell
F100$F100_CASES[is.na(F100$F100_CASES)] <- 0

# Re-order: STATE -> YEAR -> GENDER -> AGE
F100 <- F100[order(F100$STATE, F100$YEAR, F100$GENDER, F100$AGE),]
rownames(F100) <- 1:nrow(F100)

###########################################
# 3.2 ICD-10 Code F10.2
###########################################

# Delete last rows with stats sources etc.
F102 <- F102[-c(992:1001),]

# Delete empty last column
F102 <- F102[,-23]

# Delete leading and tailing whitespace
F102 <- as.data.frame(lapply(F102, function(x) trimws(x)), 
                      stringsAsFactors = FALSE)

# Recode empty cells
F102[F102 == ""] <- NA

# Rename columns
colnames(F102) <- c("YEAR", "GENDER", "AGE", F102[1,4:22])
F102 <- F102[-1,]

# Transpose STATES and rename columns
F102 <- melt(F102, id = 1:3, measured = 4:22)
colnames(F102) <- c("YEAR", "GENDER", "AGE", "STATE", "F102_CASES")

# Filling the empty cells in YEAR and GENDER column
F102$YEAR <- na.locf(F102$YEAR, na.rm = FALSE)
F102$GENDER <- na.locf(F102$GENDER, na.rm = FALSE)

# Recode GENDER and make it factor
F102$GENDER <- as.factor(F102$GENDER)
F102$GENDER <- mapvalues(F102$GENDER, from = c("Männlich", "Weiblich", 
                                               "Unbekannt"), to = c("M", "F", 
                                                                    "U"))
F102$GENDER <- factor(F102$GENDER, levels = c("M", "F", "U"))

# Recode AGE and make it factor
F102$AGE <- mapvalues(F102$AGE, agelist_name, agelist_code)
F102$AGE <- as.factor(F102$AGE)
F102$AGE <- factor(F102$AGE, levels = agelist_code)

# Recode STATE and make it factor
F102$STATE <- mapvalues(as.matrix(F102$STATE), statelist_name, statelist_code)
F102$STATE <- as.factor(F102$STATE)
F102$STATE <- factor(F102$STATE, levels = statelist_code)

# Make F102_CASES numeric
F102$F102_CASES <- as.numeric(sub(".", "", as.character(F102$F102_CASES), 
                                  fixed = TRUE))

# Fill "NA" in F102_CASES with 0, as "no cases" was coded by an empty cell
F102$F102_CASES[is.na(F102$F102_CASES)] <- 0

# Re-order: STATE -> YEAR -> GENDER -> AGE
F102 <- F102[order(F102$STATE, F102$YEAR, F102$GENDER, F102$AGE),]
rownames(F102) <- 1:nrow(F102)

###########################################
# 3.3 ICD-10 Code K70
###########################################

# Delete last rows with stats sources etc.
K70 <- K70[-c(992:1001),]

# Delete empty last column
K70 <- K70[,-23]

# Delete leading and tailing whitespace
K70 <- as.data.frame(lapply(K70, function(x) trimws(x)), 
                      stringsAsFactors = FALSE)

# Recode empty cells
K70[K70 == ""] <- NA

# Rename columns
colnames(K70) <- c("YEAR", "GENDER", "AGE", K70[1,4:22])
K70 <- K70[-1,]

# Transpose STATES and rename columns
K70 <- melt(K70, id = 1:3, measured = 4:22)
colnames(K70) <- c("YEAR", "GENDER", "AGE", "STATE", "K70_CASES")

# Filling the empty cells in YEAR and GENDER column
K70$YEAR <- na.locf(K70$YEAR, na.rm = FALSE)
K70$GENDER <- na.locf(K70$GENDER, na.rm = FALSE)

# Recode GENDER and make it factor
K70$GENDER <- as.factor(K70$GENDER)
K70$GENDER <- mapvalues(K70$GENDER, from = c("Männlich", "Weiblich", "Unbekannt"), to = c("M", "F", "U"))
K70$GENDER <- factor(K70$GENDER, levels = c("M", "F", "U"))

# Recode AGE and make it factor
K70$AGE <- mapvalues(K70$AGE, agelist_name, agelist_code)
K70$AGE <- as.factor(K70$AGE)
K70$AGE <- factor(K70$AGE, levels = agelist_code)

# Recode STATE and make it factor
K70$STATE <- mapvalues(as.matrix(K70$STATE), statelist_name, statelist_code)
K70$STATE <- as.factor(K70$STATE)
K70$STATE <- factor(K70$STATE, levels = statelist_code)

# Make K70_CASES numeric
K70$K70_CASES <- as.numeric(sub(".", "", as.character(K70$K70_CASES), fixed = TRUE))

# Fill "NA" in K70_CASES with 0, as "no cases" was coded by an empty cell
K70$K70_CASES[is.na(K70$K70_CASES)] <- 0

# Re-order: STATE -> YEAR -> GENDER -> AGE
K70 <- K70[order(K70$STATE, K70$YEAR, K70$GENDER, K70$AGE),]
rownames(K70) <- 1:nrow(K70)

# Combine health data
HEALTH <- cbind(F100, F102[!names(F102) %in% names(F100)], K70[(!names(K70) %in% names(F100)) | (!names(K70) %in% names(F102))])
HEALTH$YEAR <- as.numeric(HEALTH$YEAR)

# Remove leftovers
rm(F100, F102, K70, statelist_code, statelist_name, agelist_code, agelist_name)