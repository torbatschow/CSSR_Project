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
#     Source: Destatis: "VGR der Länder - Bruttoinlandsprodukt"
#             http://www.vgrdl.de/VGRdL/tbls/tab.jsp?rev=RV2014&tbl=tab01&lang=de-DE
#     Get Data: 
#     Clean Data:
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

###########################
# 1. Preparations
###########################

# Clear Global environment
rm(list=ls())

# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "RCurl", "ckanr", "plyr","reshape")

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

# Download population density if not in directory
if(class(try(read.csv("control/PD.csv")))=="try-error") {
url.PD <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=12411-0050&regionalschluessel=&format=csv"
write.csv(read.csv(url.PD, header = FALSE, sep=";", row.names=NULL), "control/PD.csv")
}
PD <- read.csv("control/PD.csv")

# Download unemployment rate for states (if not in directory)
if(class(try(read.csv("control/UR.csv")))=="try-error") {
  url.UR <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=13211-0007&regionalschluessel=&format=csv"
  write.csv(read.csv(url.UR, header = FALSE, sep=";", row.names=NULL), "control/UR.csv")
}
UR <- read.csv("control/UR.csv")

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

#Cleaning and Renaming PD
#ISO 3166-2:DE
PD <- PD[-c(1:6, 27:31),-c(1)]
colnames(PD) <- c("year", "DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH",
                  "DE-HE", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SL", 
                  "DE-SN", "DE-ST", "DE-SH", "DE-TH")
rownames(PD) <- c(1995:2014)
PD[,1] <- c(1995:2014)
PD <- melt(PD, id.vars = "year")
colnames(PD) <- c("YEAR", "STATE", "POD")

# Delete last rows with stats sources etc.
UR <- UR[-c(1:6,407:421),-1]

# Rename columns
colnames(UR) <- c("STATE","YEAR", "UTOTAL", "UR", "URD" ,"VACTOTAL")

# Delete leading and tailing whitespace
UR <- as.data.frame(lapply(UR, function(x) trimws(x)), 
                      stringsAsFactors = FALSE)

# Recode empty cells
UR[UR == "-"] <- NA

# Recode STATE and make it factor
UR$STATE <- mapvalues(as.matrix(UR$STATE), 
                        c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", 
                          "Bremen", "Hamburg", "Hessen", 
                          "Mecklenburg-Vorpommern", "Niedersachsen", 
                          "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", 
                          "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", 
                          "Thüringen"), 
                        c("DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH", 
                          "DE-HE", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SL", 
                          "DE-SN", "DE-ST", "DE-SH", "DE-TH"))
UR$STATE <- as.factor(UR$STATE)
UR$STATE <- factor(UR$STATE, levels = c("DE-BW", "DE-BY", "DE-BE",
                                            "DE-BB", "DE-HB", "DE-HH", 
                                            "DE-HE", "DE-MV", "DE-NI", 
                                            "DE-NW", "DE-RP", "DE-SL", 
                                            "DE-SN", "DE-ST", "DE-SH", 
                                            "DE-TH"))







