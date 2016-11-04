###########################
# Independent and control variables
# CSSR Project
# Torben&Alex
###########################

# 0. Notes
# 1. Preparation
# 2. Import Data
# 3. Clean data
# 4. Notes

##########################
# 0. Notes
##########################

# ToDO
# 0. State Area
#    Source: Destatis: 11111-0001	Gebietsfläche: Bundesländer, Stichtag
#    Ged Data: Done.
#    Clean Data: Done.
# 1. Population density 
#     Source: Destatis: "Bevölkerungsdichte: Bundesländer, Stichtag"
#     Get Data: Done
#     Clean Data: Done
# 2. Unemployment rate
#     Source: Destatis: "Arbeitslose, Arbeitslosenquoten, Gemeldete Arbeitsstellen: Bundesländer, Jahre"
#     Get Data: Done
#     Clean Data: Done
# 3. youth unemployment -
#     Source: Regionaldatenbank? 
#             difficult to find, but exists:
#             e.g. behind paywall: https://de.statista.com/statistik/daten/studie/36739/umfrage/jugendarbeitslosigkeit-nach-bundeslaendern/
#             or for single states: https://www.govdata.de/web/guest/daten/-/details/stala-sn-service-13211-004z
#             https://www.statistik.sachsen-anhalt.de/apps/StrukturKompass/indikator/zeitreihe/90
#     Get Data: 
#     Clean Data: 
# 4. state gdp
#     Source: Destatis: "VGR der Länder (Entstehungsrechnung) - Bruttoinlandsprodukt" in market prices (nothing else available)
#             https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=82111-0001&regionalschluessel=&format=csv
#     Get Data: Done.
#     Clean Data: Done.
# 5. Bildungsniveau
#     Source:
#     Get Data: 
#     Clean Data: 
# 6. Alcohol sales / Beer sales
#     Source: https://www.destatis.de/GPStatistik/receive/DEHeft_heft_00014241
#             https://www.destatis.de/GPStatistik/receive/DESerie_serie_00000146?list=all
#     Get Data: 
#     Clean Data: 
# 7. Restructure Script: by variable instead of work step    
#    Done.


###########################
# 1. Preparations
###########################

# Clear Global environment
 rm(list=ls())

# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "RCurl", "ckanr", "plyr", "reshape2")

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

###########################
# 2. Import data
###########################

# Download state area (if not in directory)
if(class(try(read.csv("control/SA.csv")))=="try-error") {
  url.SA <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=11111-0001&regionalschluessel=&format=csv"
  write.csv(read.csv(url.SA, header = FALSE, sep=";", row.names=NULL), "control/SA.csv")
}
SA <- as.data.frame(read.csv("control/SA.csv", header = T, fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))

# Download population density if not in directory
if(class(try(read.csv("control/PD.csv")))=="try-error") {
url.PD <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=12411-0050&regionalschluessel=&format=csv"
write.csv(read.csv(url.PD, header = FALSE, sep=";", row.names=NULL), "control/PD.csv")
}
PD <- as.data.frame(read.csv("control/PD.csv", header = T, fileEncoding ="ISO-8859-1", 
                              stringsAsFactors = FALSE))

# Download unemployment rate for states (if not in directory)
if(class(try(read.csv("control/UR.csv")))=="try-error") {
  url.UR <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=13211-0007&regionalschluessel=&format=csv"
  write.csv(read.csv(url.UR, header = FALSE, sep=";", row.names=NULL), "control/UR.csv")
}
UR <- as.data.frame(read.csv("control/UR.csv", header = T, fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))

# Download GDP for states (if not in directory)
if(class(try(read.csv("control/GDP.csv")))=="try-error") {
  url.GDP <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=82111-0001&regionalschluessel=&format=csv"
  write.csv(read.csv(url.GDP, header = FALSE, sep=";", row.names=NULL), "control/GDP.csv")
}
GDP <- as.data.frame(read.csv("control/GDP.csv", header = T, fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))



# youth unemployment rates


# IDEA: Use CKAN access of govdata.de
# https://cran.r-project.org/web/packages/ckanr/ckanr.pdf
# https://ropensci.org/tutorials/ckanr_tutorial.html
# Just set url
#ckanr_setup(url = " https://www.govdata.de/ckan/api")
# set url and key
#ckanr_setup(url = "http://data.techno-science.ca/", key = "my-ckan-api-key")

###########################
# 3. Clean data
###########################

###########################
# 2.0 State Area
###########################

SA <- SA[-c(1:6, 23:nrow(SA)),-c(1)]
colnames(SA) <- c("STATE", "SA")


# Recode STATE and make it factor
SA$STATE <- mapvalues(as.matrix(SA$STATE), 
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
SA$STATE <- factor(SA$STATE, levels = c("DE-BW", "DE-BY", "DE-BE",
                                        "DE-BB", "DE-HB", "DE-HH", 
                                        "DE-HE", "DE-MV", "DE-NI", 
                                        "DE-NW", "DE-RP", "DE-SL", 
                                        "DE-SN", "DE-ST", "DE-SH", 
                                        "DE-TH", "DE-West", "DE-East", 
                                        "Foreign_NA"))

# Make SA numeric
SA$SA <- as.numeric(sub(",", ".", as.character(SA$SA), 
                        fixed = TRUE))

###########################
# 2.1 Population Density
###########################

#Cleaning and Renaming PD
#ISO 3166-2:DE

# Delete leading and tailing rows / columns; rename them
PD <- PD[-c(1:5, 27:31),-c(1)]
colnames(PD) <- c("YEAR", PD[1,2:17])
PD <- PD[-1,]


# Rename YEAR rows and make it numeric
PD[,1] <- c(1995:2014)
PD$YEAR <- as.numeric(as.character(PD$YEAR))
rownames(PD) <- 1:nrow(PD)

# Transpose PD and rename columns
PD <- melt(PD, id = 1, measured = 2:17)
colnames(PD) <- c("YEAR", "STATE", "PD")

# Recode STATE and make it factor
PD$STATE <- mapvalues(as.matrix(PD$STATE), 
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
PD$STATE <- factor(PD$STATE, levels = c("DE-BW", "DE-BY", "DE-BE",
                                            "DE-BB", "DE-HB", "DE-HH", 
                                            "DE-HE", "DE-MV", "DE-NI", 
                                            "DE-NW", "DE-RP", "DE-SL", 
                                            "DE-SN", "DE-ST", "DE-SH", 
                                            "DE-TH", "DE-West", "DE-East", 
                                            "Foreign_NA"))

# Make PD numeric
PD$PD <- as.numeric(sub(".", "", as.character(PD$PD), 
                                  fixed = TRUE))



###########################
# 2.2 Unemployent Rate
###########################

# Delete leading and tailing rows / columns; re-arrange and rename
UR <- UR[-c(1:5, 6, 407:421),-c(1)]
UR <- UR[,c(2, 1, 3:6)]
colnames(UR) <- c("YEAR", "STATE", "REG_UNEMP", "UNEMP_LF_PERC", "REG_UNEMP_PERC", "REG_VACAN")
rownames(UR) <- 1:nrow(UR)

# Make YEAR numeric
UR$YEAR <- as.numeric(sub(".", "", as.character(UR$YEAR), 
                          fixed = TRUE))

# Recode STATE and make it factor
UR$STATE <- mapvalues(as.matrix(UR$STATE), 
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
UR$STATE <- factor(UR$STATE, levels = c("DE-BW", "DE-BY", "DE-BE",
                                        "DE-BB", "DE-HB", "DE-HH", 
                                        "DE-HE", "DE-MV", "DE-NI", 
                                        "DE-NW", "DE-RP", "DE-SL", 
                                        "DE-SN", "DE-ST", "DE-SH", 
                                        "DE-TH", "DE-West", "DE-East", 
                                        "Foreign_NA"))

# Make UR columns numeric
UR$REG_UNEMP <- as.numeric(sub(",", ".", as.character(UR$REG_UNEMP, fixed = TRUE)))
UR$UNEMP_LF_PERC <- as.numeric(sub(",", ".", as.character(UR$UNEMP_LF_PERC, fixed = TRUE)))
UR$REG_UNEMP_PERC <- as.numeric(sub(",", ".", as.character(UR$REG_UNEMP_PERC, fixed = TRUE)))
UR$REG_VACAN <- as.numeric(sub(",", ".", as.character(UR$REG_VACAN, fixed = TRUE)))


###########################
# 2.4 State GDP
###########################

# Delete leading and tailing rows / columns; rename them
GDP <- GDP[-c(1:6, 33:nrow(GDP)),-c(1, ncol(GDP))]
colnames(GDP) <- c("YEAR", GDP[1,2:ncol(GDP)])
GDP <- GDP[-1,]
rownames(GDP) <- 1:nrow(GDP)


# Rename YEAR rows and make it numeric
GDP$YEAR <- as.numeric(as.character(GDP$YEAR))

# Transpose GDP and rename columns
GDP <- melt(GDP, id = 1, measured = 2:17)
colnames(GDP) <- c("YEAR", "STATE", "GDP")

# Recode STATE and make it factor
GDP$STATE <- mapvalues(as.matrix(GDP$STATE), 
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
GDP$STATE <- factor(GDP$STATE, levels = c("DE-BW", "DE-BY", "DE-BE",
                                        "DE-BB", "DE-HB", "DE-HH", 
                                        "DE-HE", "DE-MV", "DE-NI", 
                                        "DE-NW", "DE-RP", "DE-SL", 
                                        "DE-SN", "DE-ST", "DE-SH", 
                                        "DE-TH", "DE-West", "DE-East", 
                                        "Foreign_NA"))

# Make GDP numeric
GDP$GDP <- as.numeric(sub(".", "", as.character(GDP$GDP), 
                        fixed = TRUE))



# Merge and delete PD, UR, SA, and GDP
INDEP <- merge(PD, UR, by = c("STATE", "YEAR"))
INDEP <- merge(INDEP, SA, by = c("STATE"))
INDEP <- merge(INDEP, GDP, by = c("STATE", "YEAR"))
remove(PD, UR, SA, GDP)
