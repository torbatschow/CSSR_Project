###########################
# Independent and control variables
# CSSR Project
# Torben&Alex
###########################

# 0. Preparation
# 1. Import Data
# 2. Clean data

###########################
# 0. Preparations
###########################

# Clear Global environment
rm(list=ls())

# Setting Working directory
try(setwd("/home/torben/GIT/Pair_Assignment_2"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/CSSR_Project"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "RCurl", "ckanr", "plyr")

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
# 1. Import data
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
#difficult to find, but exists:
# e.g. behind paywall: https://de.statista.com/statistik/daten/studie/36739/umfrage/jugendarbeitslosigkeit-nach-bundeslaendern/
# or for single states: https://www.govdata.de/web/guest/daten/-/details/stala-sn-service-13211-004z
# https://www.statistik.sachsen-anhalt.de/apps/StrukturKompass/indikator/zeitreihe/90

# IDEA: Use CKAN access of govdata.de
# https://cran.r-project.org/web/packages/ckanr/ckanr.pdf
# https://ropensci.org/tutorials/ckanr_tutorial.html
# Just set url
#ckanr_setup(url = " https://www.govdata.de/ckan/api")
# set url and key
#ckanr_setup(url = "http://data.techno-science.ca/", key = "my-ckan-api-key")



###########################
# 2. Clean data
###########################

#Cleaning and Renaming PD
#ISO 3166-2:DE
PD <- PD[-c(1:6, 27:31),-c(1)]
colnames(PD) <- c("year", "DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH", "DE-HE", "DE-MV", 
             "DE-NI", "DE-NW", "DE-RP", "DE-SL", "DE-SN", "DE-ST", "DE-SH", "DE-TH")
rownames(PD) <- c(1995:2014)
PD[,1] <- c(1995:2014)












