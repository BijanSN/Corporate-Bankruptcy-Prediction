################################
#Step 3 : Exploratory data analysis
################################
#rm(list=ls())
##############
library(ggplot2)
library(ggthemes)
library(gganimate)
library(lattice)
library(dplyr)
library(tidyr)
library(tibble)
library(magrittr)
################################
#Local functions :

survival_time_fc= function(latest_date,earliest_date){
  ld=  as.POSIXlt(latest_date)
  ed = as.POSIXlt(earliest_date)
  12 * (ld$year - ed$year) + (ld$mon - ed$mon)
}

################################
load("./_DATABASE/[Clean] Compagny_Default.RDATA")
################################
######    Key Numbers  #########

#5972 unique firms with uninterrupted monthly ratings
#710 unique defaults
Default_rate= 710/5972 #0.1189 => 11.82% 

#time frame = [1981-2017]

################################
####################
########## Ratings : 

#every ratings :

t_ratings_all= table(clean_data$ratings) #total ratings of the data sets

ggplot(clean_data, aes(clean_data$ratings )) + # colors : fill=clean_data$splticrm
  geom_bar() +
  theme_bw() 

## last compagny date ratings :

latest_data= clean_data %>%
  group_by(gvkey)       %>%
  filter(datadate==max(datadate)) 

ggplot(latest_data, aes(latest_data$ratings ))+
  geom_bar() +
  theme_bw() 


# first compagny date ratings  : remove Ds's data?
first_data= clean_data %>%
  group_by(gvkey)      %>%
  filter(datadate==min(datadate)) 

ggplot(first_data, aes(first_data$ratings ))+
  geom_bar() +
  theme_bw() 


#####################
##########Companies :

# Companies (all time) and number of occurences :
numFirms= clean_data %>% 
          count(gvkey) 


############ Determine if the compagny defaulted (at least) once or not. and if so how many times.

NumDefaultedKey= clean_data %>%
                 filter(splticrm =="D") %>%
                 # Number of firms who got a rating 'D' at least once: 
                 count(gvkey) #710 firms got D (multiple times)


NumDefaultedKey= clean_data %>%
  group_by(gvkey)           %>%
  filter(ratings =="D")    %>%
  # Number of firms who got a rating 'D' *at least* once: 
  count(gvkey) #710 firms got D (multiple times)


########## merge with clean_data : 

NumDefaultedKey<-clean_data %>%
                 separate_rows(gvkey, sep=",") 


Updated_data= clean_data %>%
              mutate(bool= ifelse(.$ratings=='D',1,0)) #%>% # create a new column "bool" =1 if the compagny defaulted
              #filter(.$bool= max(bool))










test =Updated_data %>%
      group_by(gvkey) %>%
      summarise()  %>%
      summarise(count= ifelse(.$splticrm=="D",1,0))






#https://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html?page=4


#ifelse doesn't work on factors/date
#http://mgimond.github.io/ES218/Week03a.html#combining_table_manipulation_functions_using_%%


# 
# Updated_data= clean_data %>%
#               group_by(gvkey) %>%
#               summarise(
#                 bool= ifelse(.$splticrm=='D',1,0))
# 
# 
# 





# defaulted_fc= function(rating){
#   
# if (rating) =='D'{
#   rating=1
#   else
#       ratings=0
#     end}
#     
#   
#   
# }



clean_data %>%
  group_by(gvkey) %>%
  add_column(default_bool2= defaulted_fc(.$splticrm))

















################ from gvkey to TICKER :
  #https://stackoverflow.com/questions/58983691/how-to-add-a-tibble-column-based-on-information-from-another-tibble
  #convert id from character to number
  NumDefaultedKey$gvkey<-as.integer(NumDefaultedKey$gvkey)
  
  #join tables
  newstats<-left_join(clean_data,NumDefaultedKey, by=c("gvkey" = "id"))


  #make untidy
  newstats %>%
    group_by(year) %>%
    summarize(model_id = paste0(model_id, collapse = ","), name=paste0(name, collapse = ",")) 











NewData= merge(clean_data,NumDefaultedKey, by = c("gvkey" ,"gvkey"))

? merge

#default rate of the data set:
(nrow(NumDefaultedKey)/nrow(numFirms))*100 # 11.89 % ......


#PLOTS

clean_data$splticrm = factor(clean_data$splticrm, levels=c("AAA","AA+","AA", "AA-","A+","A","A-","BBB+","BBB","BBB-", "BB+","BB","BB-","B+","B-","B","CCC+","CCC","CCC-","CC","C","D","N.M.","SD","Suspended"))

ggplot(clean_data, aes(clean_data$splticrm )) + # colors : fill=clean_data$splticrm
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


##############

ggplot(clean_data, aes(clean_data$splticrm )) +
  geom_bar() +
  theme_bw() 






clean_data = clean_data %>%
  group_by(gvkey) %>% 
  filter(datadate==max(datadate))






latestRatings= clean_data %>%
  group_by(gvkey) %>% 
  filter(datadate==max(datadate))

latestRatings %>%
  ggplot(aes(splticrm))+ #, fill=splticrm
  geom_bar() +
  theme_bw() 






#Survival time in  months :

SurvivalTime= clean_data %>% 
  group_by(gvkey) %>% 
  mutate(Survival_Time=survival_time_fc(max(datadate),min(datadate)))%>% 
  #  the most recent year shown only
  filter(datadate==max(datadate))

