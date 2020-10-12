# Depreciated code


# appy RHS function on LHS and attributes directly on LHS.
clean_data$splticrm %<>% factor 



# 
# clean_data%>% 
#   rowwise() %>% 
#   mutate(Def_bool= ifelse(.$splticrm=="D",1,0))



# clean_data %>%
#   group_by(1:n()) 





## last compagny date ratings and bool default or not.
Updated_data= Updated_data %>%
  group_by(gvkey) %>%
  filter(datadate==max(datadate)) %>%
  mutate(bool= max(bool))



# EDA
# min(clean_data$datadate)
# max(clean_data$datadate)






# Companies (all time) and number of occurences :
numFirms= clean_data %>% 
  count(gvkey) 
#pull() = vector
#select(count(.$gvkey))







# ratings colors depending if the compagny defaulted or not

ggplot(clean_data, aes(clean_data$ratings,fill=clean_data$defaultbool)) + 
  geom_bar() +
  theme_bw() 


ggplot(first_data, aes(first_data$ratings,fill=first_data$defaultbool)) + 
  geom_bar() +
  theme_bw() 


