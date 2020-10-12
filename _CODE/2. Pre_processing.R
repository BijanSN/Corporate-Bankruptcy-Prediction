########################
#Step 2 : Preprocessing
########################
#rm(list=ls())
################## Setup :
library(dplyr)
library(magrittr)
################### Loading the data :
load("./_DATABASE/[RAW] Compagny_Default.RData")
################### Preprocessing : 

clean_data=na.omit(data)                                # remove N/As
clean_data=tibble(clean_data)                           # convert into a "modern" dataframe

# str(clean_data) # we need to convert type

clean_data %<>%                                                                       #apply Right-Hand-Side functions and assigns to L.H.S.
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
            mutate(defaultbool= ifelse(ratings=='D',1,0))  %>%                     # create a new column "default_bool" =1 if the compagny defaulted
            mutate(defaultbool= max(defaultbool))

save(clean_data, file = "./_DATABASE/[Clean] Compagny_Default.RData")
