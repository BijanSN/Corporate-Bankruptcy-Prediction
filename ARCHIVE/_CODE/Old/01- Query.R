#####################################################
# Step 1 : Queries & preprocessing delisting dataset#
#####################################################

rm(list=ls())
#------------------- Setup --------------------------------------------------

library(DBI)
#library(plyr)
library(dplyr)
library(lubridate)
library(naniar)
library(birk)
library(ISLR)
library(caret)
library(pROC)
library(ModelMetrics)
# library(tidyr)
# library(tibble)
# library(magrittr)

#------------------ Queries -------------------------------------------------
# to include !! =>
#conn <- nuvolos::get_connection()


# financial_data <- dbGetQuery(conn,"SELECT GVKEY,DATADATE,FYEARQ,FQTR,ACTQ,AOQ,ATQ,CHEQ,COGSQ,CURRTRQ,CURNCDQ,CURUSCNQ,
#                                           DVPSPQ,EPSPXQ,INVTQ ,LCTQ ,LTQ ,NIQ ,OIADPQ ,PIQ ,PPENTQ ,REVTQ ,SALEQ
#                                    FROM \"FUNDQ\"")

#save(financial_data, file= "./_DATABASE/[RAW] Financial_Data.RData")


load("./_DATABASE/[RAW] Financial_Data.RData")


financial_data %<>%
                  dplyr::arrange(FYEARQ) # major data missing before row 2'700 = fiscal year 1971. 26.7% missing data
vis_miss(financial_data[1:10000,])

#CURUSCNQ = NA for non-canadian firms (exchange rate for 1 USD)

#firms with non-US childs.

financial_data %<>% mutate(CURUSCNQ = replace_na(CURUSCNQ, 1))

financial_data %<>%
                filter(FYEARQ>1975) %>% # after change : less missings !
                group_by(GVKEY) 

vis_miss(financial_data[1:10000,])
#heatmap(lapply(na.omit(financial_data, as.numeric))


financial_data %<>% 
  filter_at(vars(ACTQ,AOQ,ATQ,CHEQ), any_vars(!is.na(.))) # 6.8%
vis_miss(financial_data[1:10000,])


# vis_miss(financial_data[1:10000,])#, 'warn_large_data'= F)
# vis_miss(financial_data[10000:20000,])
# vis_miss(financial_data[20000:30000,])
# vis_miss(financial_data[30000:40115,])


glimpse(financial_data)


# modd1=lm(financial_data$NIY~financial_data$PIY + financial_data$PIQ)
# summary(modd1)


# diff PIQ et PIY = quaterly et quaterly pretax income. we need only Quaterly.

#--------------------------------------------------------------------------------------------

delist_data_raw <- dbGetQuery(conn,"SELECT PERMNO,PERMCO,HEXCD,HSICCD,DLSTDT,DLSTCD
                             FROM \"MSEDELIST\"
                             WHERE (DLSTCD like '4_%_%' OR DLSTCD = '572' OR DLSTCD = '574')")

# any delist codes which starts with 4 (liquidation code), 572 or 574 (bankrupcy code)

link_data <- dbGetQuery(conn,"SELECT CCMID, LPERMCO
                             FROM \"LINK_HISTORY\"")

link_data %<>%
  filter(LPERMCO>0) %>%
  group_by(CCMID) %>%
  unique()


delist_data_clean = delist_data_raw %>%
                                      dplyr::rename(Exchange=HEXCD) %>%
                                      dplyr::rename(SIC=HSICCD) %>%
                                      dplyr::rename(Delist_date=DLSTDT) %>%
                                      dplyr::rename(Delist_code=DLSTCD) %>% 
                                      mutate(SIC=factor(SIC)) %>% 
                                      mutate(Delist_code= factor(Delist_code))
                                      # filter(SIC <6000 & SIC >6999)
                                      # defaultbool= ifelse(ratings=='D',1,0),                                     # create a new column "default_bool" =1 if the compagny defaults at time t=i
                                      # defaultfuture = max(defaultbool),                                          # create bool if the company will default in the future.
                                      # oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0),                       # create bool =1 if defaults next year ( add "& ratings !="D" ")





delist_data_clean$Exchange=mapvalues(delist_data_clean$Exchange, from = c("1","2","3","4","5"), to =c("NYSE", "NYSE MKT", "NASDAQ", "Arca" ,"Bats"))
#delist date to the end of month ( to match the other dataset) :

delist_data_clean$Delist_date <- ceiling_date(delist_data_clean$Delist_date, "month") - days(1)



# which.closest() to match monthly delist data with quaterly ratings/quaterly delisting
# which.closest(coldate, x)

# birk::which.closest(delist_data_clean$Delist_date,financial_data$DATADATE)

glimpse(delist_data_clean)


table(delist_data_clean$Exchange) # to check!  # Header values are 1, 2, 3, 4 or 5, which correspond to the NYSE, NYSE MKT, NASDAQ, Arca and Bats respectively. Others available
# "NAICS" better than SIC

#SIC :
# 0100-0999	Agriculture, Forestry and Fishing
# 1000-1499	Mining
# 1500-1799	Construction
# 1800-1999	not used
# 2000-3999	Manufacturing
# 4000-4999	Transportation, Communications, Electric, Gas and Sanitary service
# 5000-5199	Wholesale Trade
# 5200-5999	Retail Trade
# 6000-6799	Finance, Insurance and Real Estate
# 7000-8999	Services
# 9100-9729	Public Administration
# 9900-9999	Nonclassifiable


NB_link_data = count(link_data$CCMID) # 30000+  CCMID-GVKEY links
NB_delist_data = count(delist_data_clean$PERMCO) #1615 delisted firms for bankrupcy reasons.
NB_financial_data= count(financial_data$GVKEY)# 947 unique firms after preprocessing. 1189 raw.

# gvkey= CCMID Compustat’s permanent identifier
# LPERMCO = CRSP PERMCO link

# replace CRSP's PERMCO with COMPUSTAT's CCMID/gvkey :
delist_data_clean = inner_join(delist_data_clean,link_data, by=c("PERMCO"="LPERMCO"))

delist_data_clean %<>% select(CCMID,Exchange:Delist_code) %>%  # now, the gvkey is the primary ID.
                       dplyr::rename(gvkey=CCMID)



# how many delisted firms are present in our dataset  ?


NB_link_data = count(link_data$CCMID) # 30000+  CCMID-GVKEY links
NB_delist_data = count(delist_data_clean$gvkey) #3707 delisted firms for bankrupcy reasons.
NB_financial_data= count(financial_data$GVKEY)# 944 unique firms after preprocessing. (1189 raw)


str(NB_delist_data$x)
str(NB_financial_data$x)

NB_delist_data$x=sprintf("%06d", as.numeric(NB_delist_data$x)) #converts numeric to string (character)
NB_delist_data$x=factor(NB_delist_data$x)


NB_inner= inner_join(NB_delist_data,NB_financial_data, by=c("x"="x")) #344 firms with financial info who delisted.....
# we have 344 delisted firms with quaterly financials, out of 944 ! Good !

# real data 

delist_data_clean$gvkey=sprintf("%06d", as.numeric(delist_data_clean$gvkey)) #converts numeric to string (character)
financial_data$GVKEY=factor(financial_data$GVKEY)

delist_data_clean %<>% 
                      arrange(gvkey)

financial_data %<>% 
                  arrange(GVKEY)


# inner_join : financial info of only those who delisted.
delist_data_financials_clean= inner_join(delist_data_clean,financial_data, by=c("gvkey"="GVKEY")) #1015 obs ...
count(delist_data_financials_clean$gvkey) # ...of 23 delisted firms financial infos

# dataset n°1 :
all_data_financials_clean= full_join(financial_data,delist_data_clean, by=c("GVKEY"="gvkey"))
vis_miss(all_data_financials_clean[1:10000,])


# something wrong here :
 all_data_financials_clean %<>%
                                group_by(GVKEY) %>%
                                mutate(defaultfuture2= ifelse(is.na(all_data_financials_clean$Delist_code),0,1)) %>%   # if delist code ==NA -> not on delisted dataset -> do not defaults in the future -> bool=0
                                mutate(defaultbool2= ifelse(Delist_date==DATADATE ,1,0)) %>% # defaultbool= 1 on default date
                                # mutate(oneyeardefaultpred2 = ifelse(lead(defaultbool)=='1',1,0)) %>%  # oneyeardefaultpred= 1 if the firm defaults the next year
                                # mutate(defaultfuture2= factor(defaultfuture)) %>%
                                # mutate(oneyeardefaultpred2= factor(oneyeardefaultpred))


                                
                                
  
#defaultbool = bad. only 2 obs.
#one year: even worse.

# instead: use ratings. left joins with ratings dataset.
full_data %<>% 
  group_by(DATADATE)

full_data= left_join(all_data_financials_clean, data, by=c("GVKEY"="gvkey","DATADATE"="datadate") ) 
vis_miss(full_data[1:10000,])


# to do :
# fill with non NAs. 


all_data_financials_clean$DATADATE
all_data_financials_clean$Delist_date


# Drops days (to implement before)

all_data_financials_clean$DATADATE <- format(all_data_financials_clean$DATADATE, "%Y-%m")
all_data_financials_clean$Delist_date <- format(all_data_financials_clean$Delist_date, "%Y-%m")





# model tests : 
inTrain <- createDataPartition(  # stratified random split of the data
  y = full_data$GVKEY, ## the outcome data 
  p = .75,## The percentage of data in the training set
  list = FALSE)

training <- full_data[ inTrain,]
testing  <- full_data[ -inTrain,]

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) { # to change names
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}




test2=lm(training$defaultfuture~ ACTQ+AOQ+ATQ+CHEQ+COGSQ+DVPSPQ+EPSPXQ+INVTQ+LCTQ+LTQ +NIQ +OIADPQ +PIQ +PPENTQ +REVTQ +SALEQ+ ratings,data=training)
summary(test2)

test_pred_50 = get_logistic_pred(test2, data = testing, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.5)
#change NA to "No" :
test_pred_50= ifelse(is.na(test_pred_50),0,1)  # if NA, puts 0

t=table(test_pred_50)#, useNA ="ifany")
#rownames(t) = c("Yes","No")

testing$defaultfuture=ifelse(testing$defaultfuture==1,1,0)
t2=table(testing$defaultfuture, useNA ="ifany")
rownames(t2) = c("Yes","No")

#test_tab_50 = table(predicted = t , actual =t2)

test_tab_50 = table(predicted = test_pred_50, actual =testing$defaultfuture)

test_con_mat_50 = caret::confusionMatrix(test_tab_50)#, positive = "Yes")
test_con_mat_50$byClass

#ROC
test_prob = predict(test2, newdata = testing, type = "response")
test_prob = ifelse(is.na(test_prob),0,1)


testing$defaultfuture= ifelse(is.na(testing$defaultfuture),0,1)

test_roc = roc(testing$defaultfuture ~ test_prob, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)
#AUC=1 : best. can distinguish true positive out of false negative 100% of the times.
# ROC is a curve of probability. 


args= "ACTQ ,AOQ,ATQ,CHEQ,COGSQ,CURRTRQ,CURNCDQ,CURUSCNQ,
                                          DVPSPQ,EPSPXQ,INVTQ ,LCTQ ,LTQ ,NIQ ,OIADPQ ,PIQ ,PPENTQ ,REVTQ ,SALEQ"


args3= c("ACTQ +AOQ+ATQ+CHEQ+COGSQ+ DVPSPQ+EPSPXQ+INVTQ+LCTQ+LTQ +NIQ +OIADPQ +PIQ +PPENTQ +REVTQ +SALEQ")

test1= lm(training$defaultbool~ ACTQ +AOQ+ATQ+CHEQ+COGSQ+ DVPSPQ+EPSPXQ+INVTQ+LCTQ+LTQ +NIQ +OIADPQ +PIQ +PPENTQ +REVTQ +SALEQ,data=training)
summary(test1)


test2=lm(training$defaultfuture~ ACTQ+AOQ+ATQ+CHEQ+COGSQ+DVPSPQ+EPSPXQ+INVTQ+LCTQ+LTQ +NIQ +OIADPQ +PIQ +PPENTQ +REVTQ +SALEQ+ ratings,data=training)
summary(test2)

test2=glm(full_data$defaultfuture~ ACTQ+AOQ+ATQ+CHEQ+COGSQ+DVPSPQ+EPSPXQ+INVTQ+LCTQ+LTQ +NIQ +OIADPQ +PIQ +PPENTQ +REVTQ +SALEQ,data=full_data,family = "binomial")
summary(test2)

??inTrain

inTrain <- createDataPartition(  # stratified random split of the data
  y = full_data$GVKEY, ## the outcome data 
  p = .75,## The percentage of data in the training set
  list = FALSE)

training <- full_data[ inTrain,]
training[304,]$GVKEY # 002285 =problem
training %<>% 
  filter(GVKEY!='304')

testing  <- full_data[ -inTrain,]





save(delist_data_financials_clean, file= "./_DATABASE/[Clean] Delist_Data.RData")











rm(link_data)
rm(delist_data_raw)
rm(delist_data_clean)
rm(financial_data)
rm(NB_inner)
rm(NB_delist_data)
rm(NB_link_data)
rm(NB_financial_data)
