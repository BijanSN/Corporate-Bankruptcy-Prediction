######################################################
# Step 2.2 : Pre-processing financial ratios datasets#
######################################################

rm(list=ls())

#------------------- Setup --------------------------------------------------

library(dplyr)
library(magrittr)

#-------------------Loading the data ----------------------------------------

firmfin=read.csv("./_DATABASE/[RAW] Compagny_Financials.csv")
Var_desc=read.table("./_DATABASE/Variables_Description.txt", sep="\t")   
load("./_DATABASE/[Clean] Compagny_Default.RData")

#-------------------Compagny_Financials dataset ---------------------------
Var_desc=Var_desc[-2]

firmfin %<>%
  mutate(#gvkey=factor(gvkey),                          #convert ratings into ordered factors
         public_date =as.Date(format(as.Date(public_date, format = "%Y/%m/%d"), "%Y-%m-%d")))


#firmfin$public_date <- format(as.Date(firmfin$public_date, format = "%Y/%m/%d"), "%Y-%m-%d") 

# str(firmfin$public_date)
# str(firmfin$gvkey)                                     
# str(data$gvkey) 


#gvkey from data has leading 0's, not firmfin's.

firmfin$gvkey=sprintf("%06d", as.numeric(firmfin$gvkey)) #converts numeric to string (character)
firmfin$gvkey=factor(firmfin$gvkey)

# join tests
# Joins with what's common between the two datasets (link= gvkey and date)

Compiled_data= inner_join(data,firmfin, by=c("gvkey"="gvkey","datadate"="public_date"))


# 
# testjoin2 %<>%
#   group_by(gvkey)%>%
#   filter(datadate==min(datadate))


#compiled_data_test=inner_join(data,firmfin, by=c("gvkey"="gvkey", "datadate"="public_date"))
#testjoin3= merge(data, firmfin,  by.x="gvkey", by.y="gvkey")


rm(data) # for memory efficiency
rm(firmfin) # for memory efficiency


#Simple model :
#ROE(profitability), quick ratio(liquidity),book to market (valutation) on default predictors

model1=lm(testjoin2$defaultbool ~ testjoin2$roe + testjoin2$bm + testjoin2$quick_ratio)

summary.lm(model1)
