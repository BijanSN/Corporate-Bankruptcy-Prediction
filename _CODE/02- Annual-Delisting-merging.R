# ---- ------
rm(list=ls())

library(tidyverse)
library(zoo)
library(DBI)
library(dplyr)
library(tidyr)
library(lubridate)
library(naniar)
library(birk)
library(ISLR)
library(caret)
library(pROC)
library(ModelMetrics)
library(magrittr)
library(stringr)
library(IDPmisc)
library(moments)
library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(VIM)
library(gridExtra)
library(BBmisc)

# --------------- asdasd  -------

load("./_DATABASE/[RAW] Annual_Data_no_NA.RData")
load("./_DATABASE/[CLEAN] Delist_data_clean.RData") 


#annual_data %<>% dplyr:: select(!FYEAR)# c(DVP,DVC)
annual_data %<>% mutate(DATADATE=factor(DATADATE))

annual_data %<>% group_by(GVKEY,FYEAR) %>% # select only one observaton per fiscal year
                  slice_min(1) %>%
                  ungroup()

annual_data %<>% mutate(FYEAR_1 = lag(FYEAR))
annual_data %<>% mutate(FYEAR_2 = lag(FYEAR_1))
annual_data %<>% mutate(FYEAR_3 = lag(FYEAR_2))
annual_data %<>% mutate(FYEAR_4 = lag(FYEAR_3))

# DELIST DATA 
delist_data_clean$Delist_date <-format(delist_data_clean$Delist_date, "%Y")

#delist_data_clean %<>%  dplyr::select(!c(SIC,Exchange))

#FISCAL YEAR 
#obs_delisted= inner_join(delist_data_clean,annual_data, by=c("gvkey"="GVKEY", "Delist_date"="FYEAR")) 
# Merge ratios & delist dataset, only keep delisted (defaulted) companies (431)


#1 year prior bankruptcy observations of all delisted firms:
obs_delisted= inner_join(delist_data_clean,annual_data, by=c("gvkey"="GVKEY", "Delist_date"="FYEAR_1"))
obs_delisted_1= inner_join(delist_data_clean,annual_data, by=c("gvkey"="GVKEY", "Delist_date"="FYEAR"))

# (319) OBS of delisted firms ON delisting YEAR. 208 on fiscal Y-1

nb_firms = obs_delisted %>%  group_by(gvkey) %>%  slice_min(1) # of 202 firms.

obs_delisted %<>%  mutate(pred_default1= 1)
obs_delisted %<>%  mutate(pred_default1= ifelse(is.na(pred_default1),0,1))  #none obviously

# stargazable  ::::::
all_obs_ratios= inner_join(delist_data_clean,annual_data, by=c("gvkey"="GVKEY"))  # Merge ratios & delist dataset, only keep delisted (defaulted) companies
#(14000) obs of delisted firms before and on delisted year.

FULL_MODEL_DATA= left_join(all_obs_ratios,obs_delisted)
#FULL_MODEL_DATA %<>%  distinct()

FULL_MODEL_DATA %<>%  mutate(pred_default1= ifelse(is.na(pred_default1),0,1))
ONLY_DELISTED_T_1= obs_delisted


ggplot(ONLY_DELISTED_T_1, aes(x=Delist_date)) +
  geom_histogram(bins=5,stat="count")



nb=ONLY_DELISTED_T_1 %>%  dplyr::select(gvkey) %>%   # 202 firms,208 obs 1 yer prior delisting.
  unique()


TOTAL_DELISTED=left_join(nb, FULL_MODEL_DATA) #total = 4816 obs of delisted firms 
#to predict : 

TOTAL_DELISTED %<>% group_by(gvkey, FYEAR) #
  
table(TOTAL_DELISTED$pred_default1)# 1.1% of the observation defaults.

 # 204/4508 = 4.5% DEFAULT

save(TOTAL_DELISTED, ONLY_DELISTED_T_1, file="./_DATABASE/[CLEAN] MODEL_FINAL_data.RData")

