############
# R-File Get and Clean Data for Assignment 1
# Torben and Alexander
# Updated September 30, 2016
###########

#Set Working Directory
try(setwd("/home/torben/GIT/1st-Pair-Assignment"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Collaborative Social Science Data Analysis/1st-Pair-Assignment"), silent = TRUE)

#######################
#Get Data
#Source: https://github.com/fivethirtyeight/data
########################
#Police killings
policeKilling <- read.csv(text = getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/police-killings/police_killings.csv"))

#US births 2000 - 2014
#Dataset from: https://github.com/fivethirtyeight/data/tree/master/births
births <- read.csv(text = getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv"))

#######################
#Data Preparation: Police Killing
######################

#list of factors
cause.level <- levels(policeKilling$cause)
#count the causes
cause.count <- count(policeKilling, "cause")[,2]
#put them together in a df
cause.df <- data.frame(cause.level, cause.count)

#######################
#Data Preparation US births 2000 - 2014
#######################

#Generate lists with days of week and months
l_days_of_week <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
l_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#Re-label births variables and make them factors
births$day_of_week <- factor(births$day_of_week, levels = 1:7, labels = l_days_of_week)
births$month <- factor(births$month, levels = 1:12, labels = l_months)

#Generate matrix with births on specific days of week and specific months (Matrix is needed for heatmap.)
mat = matrix(nrow = 12, ncol = 7)
rownames(mat) <- l_months
colnames(mat) <- l_days_of_week
for (m in l_months) {
  for (wd in l_days_of_week) {
    mat[m, wd] <- Reduce("+", births[(births$month == m) & (births$day_of_week == wd), 5])
  }
}






