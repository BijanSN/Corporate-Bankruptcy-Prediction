#############################################
# Step 2.1 : Pre-processing ratings dataset #
#############################################

rm(list=ls())

#------------------- Setup --------------------------------------------------

library(dplyr)
library(magrittr)

#-------------------Loading the data ----------------------------------------

load("./_DATABASE/[RAW] Compagny_Default.RData")

##----------------- Rating dataset & default booleans -----------------------


data %<>%                                                                                  # pipe opperator "%<>%" applies the right-hand-side functions and assigns to L.H.S.
         na.omit() %>%                                                                     # x %>% f(y) =  f(x,y). removes uncomplete data
         rename(ratings=splticrm) %>%                                                      # rename to something more clear
         filter(!ratings %in% c("N.M.","SD","Suspended")) %>%                              # remove unusable ratings
         mutate(gvkey=factor(gvkey)) %>%
         group_by(gvkey) %>%
  
         mutate(datadate=as.Date(datadate),                                                # convert from 'numeric' to 'Date' type
                ratings= factor(ratings,levels=c("AAA","AA+","AA", "AA-",
                                                              "A+","A","A-","BBB+","BBB",
                                                              "BBB-", "BB+","BB","BB-",
                                                              "B+","B-","B","CCC+","CCC",
                                                              "CCC-","CC","C","D")),       # convert from 'character' to 'factor' type, in quality's descending order

                defaultbool= ifelse(ratings=='D',1,0),                                     # create a new column "default_bool" =1 if the compagny defaults at time t=i
                defaultfuture = max(defaultbool),                                          # create bool if the company will default in the future.
                oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0),                       # create bool =1 if defaults next year ( add "& ratings !="D" ")
                       
                defaultdate=case_when(defaultfuture==1 ~min(datadate),                     # create a new column with the default date, if applicable
                                      defaultfuture==0 ~as.Date('2100-12-12'))) %>%        # can't put 'NA' directly : I use a fake date to convert afterwards.
        

          droplevels()                                                                     # remove unused factors
                                                                        
data$defaultdate[data$defaultdate ==as.Date("2100-12-12")] = NA                            # defaultdate = 'NA' if no default date


save(data, file = "./_DATABASE/[Clean] Compagny_Default.RData")

# TO DO : 3 defaults ratings in a row (persistent).


#--------------------- Compagny_Financials dataset ---------------------------

# Financial Ratios Firm Level by WRDS

numFirms= data %>% 
                  count(gvkey) #counts the number of individual firm defaults and lists the key of each companies.

write.table(numFirms$gvkey,"./_DATABASE/Compagny_gvkey.txt", sep = "/n",row.names = F,
                                                             col.names = F, quote = F)    # write the keys to a txt file
                                                                                          #(Manual) web query using "Compagny_gvkey.txt" => Creates : "[RAW] Compagny_Financials.csv"
