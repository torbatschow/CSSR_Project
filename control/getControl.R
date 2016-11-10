###########################
# Independent and control variables
# CSSR Project
# Torben&Alex
# Date: 07.11.2016
###########################

# 0. Notes
# 1. Preparation
# 2. Population / State area
# 3. Population density
# 4. Unemployment rate
# 5. GDP of states
# 6. Beer tax
# 7. Youth unemployment
# 8. Education
# 9. Merge data


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
#             Hertie has a campus access :)
#             or for single states: https://www.govdata.de/web/guest/daten/-/details/stala-sn-service-13211-004z
#             https://www.statistik.sachsen-anhalt.de/apps/StrukturKompass/indikator/zeitreihe/90
#     Get Data: Done
#     Clean Data: Done
# 4. state gdp
#     Source: Destatis: "VGR der Länder (Entstehungsrechnung) - Bruttoinlandsprodukt" in market prices (nothing else available)
#             https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=82111-0001&regionalschluessel=&format=csv
#     Get Data: Done.
#     Clean Data: Done.
# 5. ***ACHTUNGACHTUNGACHTUNG*** Bildungsniveau - Absolventen/Abgänger nach dem Schulabschluss ***ACHTUNGACHTUNGACHTUNG***
#     Source: Regionaldatenbank GENESIS: 192-71-4-B	Allgemeinbildende Schulen: Absolventen/Abgänger nach dem Schulabschluss - Schuljahr -  regionale Ebenen
#             Only the graduates per YEAR, but maybe of use for for youth analysis...
#     Get Data: DONE.
#     Clean Data: DONE.
# 6. Beer Tax
#     Source: Genesis-online: 71211-0007	Steuereinnahmen: Bundesländer, Quartale, Steuerarten vor der Steuerverteilung
#             https://www.destatis.de/GPStatistik/receive/DEHeft_heft_00014241
#             https://www.destatis.de/GPStatistik/receive/DESerie_serie_00000146?list=all
#     Get Data: Done.
#     Clean Data: Done.
# 7. Population
#    Source: https://www.govdata.de/web/guest/suchen/-/details/destatis-service-12211-0005
#     Only data set I found with a complete timeline
#    Get Data: Done
#    Clean Data: Done
# 8. Restructure Script: by variable instead of work step    
#    Done.


###########################
# 1. Preparations
###########################

# Clear Global environment
#rm(list=ls())

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

# Generate list of states
statelist_name <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", 
                    "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", 
                    "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", 
                    "Saarland", "Sachsen", "Sachsen-Anhalt", 
                    "Schleswig-Holstein", "Thüringen")

statelist_name_noU <- c("baden-wuerttemberg", "bayern", "berlin", "brandenburg", 
                    "bremen", "hamburg", "hessen", "mecklenburg-vorpommern", 
                    "niedersachsen", "nordrhein-westfalen", "rheinland-pfalz", 
                    "saarland", "sachsen", "sachsen-anhalt", 
                    "schleswig-holstein", "thueringen")

statelist_code <- c("DE-BW", "DE-BY", "DE-BE", "DE-BB", "DE-HB", "DE-HH", 
                    "DE-HE", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SL",
                    "DE-SN", "DE-ST", "DE-SH", "DE-TH")

###########################
# 2. Population / State Area
###########################

# Download state area (if not in directory)
if(class(try(read.csv("control/SA.csv")))=="try-error") {
  url.SA <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=11111-0001&regionalschluessel=&format=csv"
  write.csv(read.csv(url.SA, header = FALSE, sep=";", row.names=NULL), "control/SA.csv")
}
SA <- as.data.frame(read.csv("control/SA.csv", header = T,
                             fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))

SA <- SA[-c(1:6, 23:nrow(SA)),-c(1)]
colnames(SA) <- c("STATE", "SA")

# Recode STATE and make it factor
SA$STATE <- mapvalues(as.matrix(SA$STATE), statelist_name, statelist_code)
SA$STATE <- factor(SA$STATE, levels = statelist_code)

# Make SA numeric
SA$SA <- as.numeric(sub(",", ".", as.character(SA$SA), 
                        fixed = TRUE))

# Download population(if not in directory)
if(class(try(read.csv("control/PP.csv")))=="try-error") {
  url.PP <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=12211-0005&regionalschluessel=&format=csv"
  write.csv(read.csv(url.PP, header = FALSE, sep=";", row.names=NULL),
            "control/PP.csv")
}
PP <- as.data.frame(read.csv("control/PP.csv", header = T,
                             fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))

# remove source info and id column
PP <- PP[-c(1:7),-1]

# add variable names
colnames(PP) <- c("STATE", "YEAR", "PP", "PP.EM", "PP.UE", "PP.EA", "PP.EIA")

# streamline year column
PP[,2] <- as.numeric(substring(as.character(PP[,2]),0,4))

# Recode STATE and make it factor
PP$STATE <- mapvalues(as.matrix(PP$STATE), statelist_name, statelist_code)
PP$STATE <- factor(PP$STATE, levels = statelist_code)
PP$YEAR <- factor(PP$YEAR, levels = 1991:2015)
for (i in 3:ncol(PP)){
  PP[,i] <- as.numeric(PP[,i])
}

#############################
# 3. Population Density
############################

# Download population density if not in directory
if(class(try(read.csv("control/PD.csv")))=="try-error") {
url.PD <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=12411-0050&regionalschluessel=&format=csv"
write.csv(read.csv(url.PD, header = FALSE, sep=";", row.names=NULL), 
          "control/PD.csv")
}
PD <- as.data.frame(read.csv("control/PD.csv", header = T,
                             fileEncoding ="ISO-8859-1", 
                             stringsAsFactors = FALSE))

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
PD$STATE <- mapvalues(as.matrix(PD$STATE), statelist_name, statelist_code)
PD$STATE <- factor(PD$STATE, levels = statelist_code)

# Make PD numeric
PD$PD <- as.numeric(sub(".", "", as.character(PD$PD), 
                        fixed = TRUE))

###########################
# 4. Unemployment rate
###########################

# Download unemployment rate for states (if not in directory)
if(class(try(read.csv("control/UR.csv")))=="try-error") {
  url.UR <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=13211-0007&regionalschluessel=&format=csv"
  write.csv(read.csv(url.UR, header = FALSE, sep=";", row.names=NULL),
            "control/UR.csv")
}
UR <- as.data.frame(read.csv("control/UR.csv", header = T,
                             fileEncoding ="ISO-8859-1",
                             stringsAsFactors = FALSE))

# Delete leading and tailing rows / columns; re-arrange and rename
UR <- UR[-c(1:5, 6, 407:421),-c(1)]
UR <- UR[,c(2, 1, 3:6)]
colnames(UR) <- c("YEAR", "STATE", "UTOTAL", "UR.LF", "UR", "VAC")
rownames(UR) <- 1:nrow(UR)

# Make YEAR numeric
UR$YEAR <- as.numeric(sub(".", "", as.character(UR$YEAR), 
                          fixed = TRUE))

# Recode STATE and make it factor
UR$STATE <- mapvalues(as.matrix(UR$STATE), statelist_name, statelist_code)
UR$STATE <- factor(UR$STATE, levels = statelist_code)

# Make UR columns numeric
UR$UTOTAL <- as.numeric(sub(",", ".", as.character(UR$UTOTAL, fixed = TRUE)))
UR[UR == "-"] <- NA
UR$UR.LF <- as.numeric(sub(",", ".", as.character(UR$UR.LF, fixed = TRUE)))
UR$UR <- as.numeric(sub(",", ".", as.character(UR$UR, fixed = TRUE)))
UR$VAC <- as.numeric(sub(",", ".", as.character(UR$VAC, fixed = TRUE)))


############################
# 5. GDP of states
############################

# Download GDP for states (if not in directory)
if(class(try(read.csv("control/GDP.csv")))=="try-error") {
  url.GDP <- "https://www-genesis.destatis.de/genesis/online?sequenz=tabelleDownload&selectionname=82111-0001&regionalschluessel=&format=csv"
  write.csv(read.csv(url.GDP, header = FALSE, sep=";", row.names=NULL),
            "control/GDP.csv")
}
GDP <- as.data.frame(read.csv("control/GDP.csv", header = T,
                              fileEncoding ="ISO-8859-1",
                              stringsAsFactors = FALSE))

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
GDP$STATE <- mapvalues(as.matrix(GDP$STATE), statelist_name, statelist_code)
GDP$STATE <- factor(GDP$STATE, levels = statelist_code)

# Make GDP numeric
GDP$GDP <- as.numeric(sub(".", "", as.character(GDP$GDP), 
                          fixed = TRUE))

##############################
# 6. Beer Tax 
############################

# Import Beer Tax for states
BTAX <- as.data.frame(read.csv2("control/BTAX.csv", header = F,
                                fileEncoding ="ISO-8859-1",
                                stringsAsFactors = FALSE))
# Delete leading and tailing rows / columns; rename them
BTAX <- BTAX[-c(1:7, 1160:nrow(BTAX)),]
colnames(BTAX) <- c("STATE", "YEAR", "Q", "BTAX")
rownames(BTAX) <- 1:nrow(BTAX)

# Sum quarterly numbers to yearly
BTAXSUM <- data.frame("STATE", "YEAR", "BTAX", stringsAsFactors = FALSE)
for (state in statelist_code){
  for (year in 1999:2016){
    BTAXSUM <- rbind(BTAXSUM, c(state, year, NA))
  }
}
colnames(BTAXSUM) <- BTAXSUM[1,]
BTAXSUM <- BTAXSUM[-1,]
BTAX[BTAX == "..."] <- NA
BTAXSUM$BTAX <- aggregate(as.numeric(BTAX$BTAX), by = list(BTAX$YEAR, BTAX$STATE),
                          FUN = sum)[[3]]
BTAX <- BTAXSUM
rm(BTAXSUM)

# Recode STATE and make it factor
BTAX$STATE <- factor(BTAX$STATE, levels = statelist_code)

# Rename YEAR rows and make it numeric
BTAX$YEAR <- as.numeric(as.character(BTAX$YEAR))

# Make BTAX numeric
BTAX$BTAX <- as.numeric(sub(".", "", as.character(BTAX$BTAX), 
                            fixed = TRUE))


#############################
# 7. Youth unemployment
#############################

# list of all excel files from statista
YUR.L <- list.files ("control",recursive = TRUE, pattern = "^statistic")

# add state column to link list
YUR.L <- cbind(LINK = YUR.L, STATE = gsub("-bis-2015.xlsx" ,"" , substring(YUR.L,71)))
YUR.L <- as.data.frame(YUR.L)
YUR.L$STATE <- mapvalues(as.matrix(YUR.L$STATE), statelist_name_noU, statelist_code)

# youth unemployment data frame
YUR <- data.frame(NA, NA, NA)
YUR[,1] <- as.character(YUR[,1])
YUR[,2] <- as.numeric(YUR[,2])
YUR[,3] <- as.numeric(YUR[,3])
colnames(YUR) <- c("STATE", "YEAR", "YUR")

# load, clean and merge 16 excel files
for (i in 1:16) {
  tmp <- read_excel(paste("control/",YUR.L[YUR.L[,2] == statelist_code[i]][1],sep = ""),
                    sheet = 2)
  tmp <- tmp[c(3:25), c(1:2)]
  for (j in as.numeric(unlist(tmp[,1]))){
    YUR <- rbind(YUR, c(statelist_code[i], j, as.numeric(tmp[tmp[,1] == j,2])))
  }
  remove(tmp)
}
YUR <- YUR[-1,]

#############################
# 8. Education
#############################

# Import Education for states
EDU <- as.data.frame(read.csv2("control/EDU.csv", header = F,
                     fileEncoding ="ISO-8859-1", stringsAsFactors = FALSE))

# Import Education for states
EDU <- as.data.frame(read.csv2("control/EDU.csv", header = F, 
                     fileEncoding ="ISO-8859-1", stringsAsFactors = FALSE))

# Delete leading and tailing rows / columns; rename them
EDU <- EDU[-c(1:7, 9, 331:nrow(EDU)),-c(2)]
rownames(EDU) <- 1:nrow(EDU)
EDU <- EDU[,-c(4, 6, 8, 10, 12, 14)]
colnames(EDU) <- c("YEAR", "STATE", "EDU_TOTAL", "EDU_NO", "EDU_HS", "EDU_RS",
                   "EDU_FH", "EDU_AH")
EDU <- EDU[-c(1:2),]
EDU <- EDU[, c(2, 1, 3:ncol(EDU))]

# Rename YEAR rows and make it numeric
EDU$YEAR <- as.numeric(as.character(EDU$YEAR))
rownames(EDU) <- 1:nrow(EDU)

# Recode STATE and make it factor
EDU$STATE <- mapvalues(as.matrix(EDU$STATE), "Freistaat Sachsen", "Sachsen")
EDU$STATE <- mapvalues(as.matrix(EDU$STATE), statelist_name, statelist_code)
EDU$STATE <- factor(EDU$STATE, levels = statelist_code)

# Make EDU numeric
for (i in 3:8){
  EDU[,i] <- as.numeric(sub(".", "", as.character(EDU[,i]), 
                            fixed = TRUE))
}


###########################
# 9. Merge Data
###########################


# Merge and delete PD, UR, SA, GDP, BTAX
INDEP <- merge(PD, UR, by = c("STATE", "YEAR"))
INDEP <- merge(INDEP, PP, by= c("STATE", "YEAR"))
INDEP <- merge(INDEP, SA, by = c("STATE"))
INDEP <- merge(INDEP, GDP, by = c("STATE", "YEAR"))
INDEP <- merge(INDEP, BTAX, by = c("STATE", "YEAR"))
INDEP <- merge(INDEP, YUR, by = c("STATE", "YEAR"))
INDEP <- merge(INDEP, EDU, by = c("STATE", "YEAR"))
remove(PD, PP, UR, SA, GDP, BTAX, YUR, EDU, YUR.L, i, j, state, statelist_code,
       statelist_name, statelist_name_noU, year)
