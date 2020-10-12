##########################

#Exploratory data analysis

##########################

rm(list=ls())
############
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gganimate)
library(lattice)

load("Clean_Compagny_Default.RDATA")

numFirms= clean_data %>% count(gvkey) #5972 firms with uninterrupted monthly ratings

# Number of firms who got a rating 'D' at least once
NumDefaultedKey= clean_data %>%
  filter(splticrm =="D") %>%
  count(gvkey) #710 firms got D   


#defaultedKey= clean_data %>%
#  filter(splticrm =="D") %>%
#  #filter() 
#  group_by(gvkey, datadate)


#default rate of the data set:
(nrow(NumDefaultedKey)/nrow(numFirms))*100 # 12% ......


clean_data$splticrm = factor(clean_data$splticrm, levels=c("AAA","AA+","AA", "AA-","A+","A","A-","BBB+","BBB","BBB-", "BB+","BB","BB-","B+","B-","B","CCC+","CCC","CCC-","CC","C","D","N.M.","SD","Suspended"))

ggplot(clean_data, aes(clean_data$splticrm, fill=clean_data$splticrm )) + #enlever fill pour colors
  geom_bar() +
  theme_bw() 
# +
# transition_states(
#   clean_data$datadate,
#   transition_length = 2,
#   state_length = 1
# ) +
# ease_aes('sine-in-out')
#anim_save("animated-barplot-transition.gif")


latestRatings= clean_data %>%
  group_by(gvkey) %>% 
  filter(datadate==max(datadate))

View(table(latestRatings))

latestRatings %>%
  ggplot(aes(splticrm, fill=splticrm))+
  geom_bar() +
  theme_bw() 









