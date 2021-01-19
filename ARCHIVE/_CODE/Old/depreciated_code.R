# Depreciated code




#03- Merge : 
  # inner_join : financial info of only those who delisted :
  
  # delisted_financial_quarter= inner_join(delist_data_clean,financial_data, by=c("gvkey"="GVKEY", "delist_year_quarter"="financial_year_quarter")) #378 firms with financial info on the quarter they delisted ! the ones to predict !
  # # let's add them a bool
  # delisted_financial_quarter = delisted_financial_quarter %>%  
  #                              mutate(pred_default1= 1)
  
  #------------------------------------------------------------

# # ou genéral et filtre avec condition et add un bool... genre
# delisted_financial_quarter= inner_join(delist_data_clean,financial_data, by=c("gvkey"="GVKEY")) #378 firms with financial info on the quarter they delisted ! the ones to predict !
# # 44925 obs.
# # we have 378 delisted firms with quaterly financials ( out of ~944 financial$gvkey) ! 3.1% missing values. Good !

# delisted_financial_quarter= delisted_financial_quarter %>% 
#                                 filter(delist_year_quarter==financial_year_quarter) %>%      # filter such that the observation is the quarter where it defaults
#                                 mutate(pred_default1= 1) %>%                                 # adds the bool only on default date 
#                                 right_join(delisted_financial_quarter)                       # join with unfiltered data







  
  #how many defaultsdate=defaultdate ( financial when info are available) ?
  #delisted_financial_quarter_default = delisted_financial_quarter %>% filter(delist_year_quarter==financial_year_quarter) # 386 obs on default, out of 44933. 0.8% occurence of defaults
  
  #nb=unique(delisted_financial_quarter_default$gvkey) #369 firms, 1mio obs.






























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




linktable2=read.csv("./_DATABASE/Corporate_Data.csv")

numfirms2=linktable2 %>%
  count(GVKEY) 
#2632 firms 







linktable=read.csv("./_DATABASE/compagny_ID.csv")
linktable$gvkey






z= numfirms2$GVKEY %in% NumDefaultedKey$gvkey
table(z)
#100 






clean_data %<>%                                                                       # %<>% : applies Right-Hand-Side functions and assigns to L.H.S.
  filter(!splticrm %in% c("N.M.","SD","Suspended")) %>%                     #remove unusable ratings
  mutate(datadate=as.Date(.$datadate)) %>%                                  #from numeric to date type
  mutate(splticrm= factor(.$splticrm,levels=c("AAA","AA+","AA", "AA-",
                                              "A+","A","A-","BBB+","BBB",
                                              "BBB-", "BB+","BB","BB-",
                                              "B+","B-","B","CCC+","CCC",
                                              "CCC-","CC","C","D"))) %>%              #convert ratings into ordered factors
  rename(ratings=splticrm) %>%                                              #rename to something more clear
  droplevels()                                                              #drop unused factors


clean_data %<>%
  group_by(gvkey) %>%
  mutate(defaultbool= ifelse(ratings=='D',1,0),                         # create a new column "default_bool" =1 if the compagny defaults at time t=i
         defaultfuture = max(defaultbool),                              # create bool if the company will default in the future.
         oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0),           # create bool =1 if defaults next year ( add "& ratings !="D" ")
         defaultdate=case_when(defaultfuture==1 ~min(datadate),         # create a new column with the default date, if applicable
                               defaultfuture==0 ~as.Date('2100-12-12'))) # can't put 'NA' directly : I use a fake date to convert afterwards.

clean_data$defaultdate[clean_data$defaultdate ==as.Date("2100-12-12")] = NA

