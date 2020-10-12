########################

#Step 2 : Preprocessing

########################

rm(list=ls())
############
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gganimate)
library(lattice)
#library(gifski)

setwd("C:/Users/Vida/Desktop/Thèse/NEW- Credit risk/DATABASE")

#Loading the data :

load("Compagny_Default.RData")

data=data.frame(data, stringsAsFactors = F)
data=data[,c('gvkey','datadate','splticrm')]

clean_data=na.omit(data)
clean_data=as_tibble(clean_data)

clean_data= clean_data  %>%
  mutate(datadate=as.Date(datadate))

save(clean_data, file = "Clean_Compagny_Default.RData")


#remove N/As (next : look out for outliers !)