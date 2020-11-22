##############################
# Step 3 : Merging databases #
##############################

rm(list=ls())

# run sections with Ctrl+Alt+T

# Motivation :

# Find best model( coef+ stat method) to predict default.
# USING companies who defaulted ! 

#--------Setup------------------------------------------------

library(zoo)
library(DBI)
library(dplyr)
library(lubridate)
library(naniar)
library(birk)
library(ISLR)
library(caret)
library(pROC)
library(ModelMetrics)
library(magrittr)
library(reshape2)
library(ggplot2)
#library(plyr)

#--------Loading datasets------------------------------------

#load("./_DATABASE/[CLEAN] Financial_Data.RData")           # financial dataset
load("./_DATABASE/[CLEAN] Delist_data_clean.RData")        # delisted dataset
load("./_DATABASE/[Clean] Compagny_Default.RData")         # ratings dataset
load("./_DATABASE/[CLEAN] Ratio_Data.RData")               # subset of financial dataset, to run models on

#------------------------------------------------------------

delisted_companies_ratios_by_quarter= inner_join(delist_data_clean,ratios_data, by=c("gvkey"="GVKEY"))               # Merge ratios & delist dataset, only keep delisted (defaulted) companies

# select only SIC=/= financial companies to do here or before ! 


#  ONLY DELISTED companies ON DEFAULT QUARTER dataset:
delisted_companies_ratios_on_default_quarter= inner_join(delist_data_clean,ratios_data, by=c("gvkey"="GVKEY", "delist_year_quarter"="financial_year_quarter"))
# 198 occurences to predict.


#  ONLY DELISTED companies ALL TIME dataset: (+ add default boolean if quarter date= default date)
delisted_companies_ratios_by_quarter= delisted_companies_ratios_by_quarter %>% 
                                                        filter(delist_year_quarter==financial_year_quarter) %>%      # filter such that the observation is the quarter where it defaults
                                                        mutate(pred_default1= 1)                            %>%      # adds the bool only on default date 
                                                        right_join(delisted_companies_ratios_by_quarter)             # join back with unfiltered data


delisted_companies_ratios_by_quarter %<>% mutate(pred_default1= ifelse(is.na(pred_default1),0,1))                    # replace bool's NA with 0s
#table(delisted_companies_ratios_by_quarter $pred_default1)# 1.1% of the observation defaults.

delisted_companies_ratios_by_quarter %<>% group_by(gvkey)%>%
                                          arrange(gvkey,financial_year_quarter) 

# vis_miss(delisted_companies_ratios_by_quarter[1:10000,]) # no Nas anymore


delisted_companies_ratios_by_quarter %<>% 
                                        filter_all(all_vars(!is.infinite(.)))


save(delisted_companies_ratios_by_quarter, file= "./_DATABASE/[CLEAN] Delisted_ratio.RData")

#---------------- After preprocessing - Data description -------------


load("./_DATABASE/[CLEAN] Delisted_ratio.RData")

##-------- How many firms ?

companies_occurences= delisted_companies_ratios_by_quarter %>% 
                                                              select(gvkey) %>% 
                                                              summarise(counts = n()) 
#721 unique DELISTED gvkeys. 


##--------- Number of obs ? 

# 17100 quaterly observation of DELISTED companies.

# How many quaterly observations per gvkeys, on average ?

#hist(companies_occurences$counts), on base R
companies_occurences %>%  ggplot(., aes(counts)) +
                          geom_histogram()

mean(companies_occurences$counts) # mean = 23 quarters:  but data is heavily right-skewed : median better
median(companies_occurences$counts) # the median is 14 quarters of data per compagny

# Exemple : GVKEY 008515.From 1988 to 2019.


##--------- How many unique defaulted firms ? 

defaults_by_gvkey= delisted_companies_ratios_by_quarter %>% group_by(gvkey) %>% 
                                                            filter(pred_default1==1)
#194 DELISTED companies actually DEFAULTED in our dataset.



#------------------ Data visualisation on coefficients : ---------------
# (faire visu boxplot mais avec x=factor(predict_default) pour savoir si un truc change si default ou pas !)

glimpse(delisted_companies_ratios_by_quarter)
# see if log scales useful or not / avoid outliers

# WCAPQ_ATQ
delisted_companies_ratios_by_quarter %>% 
ggplot(.,aes(x="",y=WCAPQ_ATQ)) + 
geom_boxplot(outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 3) 

#EBIT
delisted_companies_ratios_by_quarter %>% 
  ggplot(.,aes(x="",y=EBIT)) + 
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 3) 


#pred default as factor on EBIT

delisted_companies_ratios_by_quarter %>% 
  ggplot(.,aes(x=as.factor(pred_default1),y=EBIT)) + 
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 3) 


#----- create more default predictors ----------------------------

delisted_financial %<>%
                        group_by(GVKEY) %>%
  
                        mutate(defaultfuture_delist= ifelse(is.na(Delist_date),0,1)) %>%   # if delist code ==NA -> not on delisted dataset -> do not defaults in the future -> bool=0
                        mutate(defaultbool_delist= ifelse(DATADATE==Delist_date ,1,0))  # defaultbool= 1 on default date. # use delist_date from delist database.

                        #defaultfuture : on "ratings" dataset
                        #oneyeardefaultpred : to change in year and not months or quarter as i did

                        # mutate(oneyeardefaultpred2 = ifelse(lead(defaultbool)=='1',1,0)) %>%  # oneyeardefaultpred= 1 if the firm defaults the next year
                        # mutate(defaultfuture2= factor(defaultfuture)) %>%
                        # mutate(oneyeardefaultpred2= factor(oneyeardefaultpred))
                        

# change predictor such as = 1 if next 4 quarters= defaults
# another one if 8 next predictors, etc...

#-------------------------------------------------------------------
# do more preprocessing, create ratios, etc...

#---------Visualisation of the defaulted companies --------------------

glimpse(delisted_companies_ratios_by_quarter)

# Observations 


# Data_visualisation= delisted_companies_ratios_by_quarter %>%  select(-Exchange,-Delist_date, -Delist_code,-SIC)
# 
# glimpse(Data_visualisation)    
# Dates % SIC : bar plots  
# Numerical variables : boxplots



# histogram :

hist(companies_occurences[,2])

boxplot(companies_occurences[,2])





# --------remove INF of ratios !----------



#-----------------------------------------

delisted_companies_ratios_by_quarter %<>% mutate(pred_default1= ifelse(is.na(pred_default1),0,1))
table(delisted_companies_ratios_by_quarter $pred_default1)# 1.1% obs defaults.

#View(delisted_companies_ratios_by_quarter)

# coef_names=colnames(delisted_companies_ratios_by_quarter)
# print(coef_names)

# for motivation, let's do the models!

logistic_regression_ratios <- glm(pred_default1~ financial_year_quarter+WCAPQ_ATQ+REQ_ATQ+EBIT+CA_CL+CH_CL+CA_TD+WC_TA+WC_SAL+NIQ_TA+RE_TA+EBIT_TA, data=delisted_companies_ratios_by_quarter, family = "binomial")
