######################################################
# Step 2.2 : Pre-processing financial ratios datasets#
######################################################

rm(list=ls())

#------------------- Setup --------------------------------------------------

library(dplyr)
library(magrittr)
library(anomalize)

set.seed(100)

#-------------------Loading the data ----------------------------------------

firmfin=read.csv("./_DATABASE/[RAW] Compagny_Financials.csv")
Var_desc=read.table("./_DATABASE/Variables_Description.txt", sep="\t")   
load("./_DATABASE/[Clean] Compagny_Default.RData")

#-------------------Compagny_Financials dataset ---------------------------
Var_desc=Var_desc[-2]

firmfin %<>%
  mutate(#gvkey=factor(gvkey),                          #convert ratings into ordered factors
         public_date =as.Date(format(as.Date(public_date, format = "%Y/%m/%d"), "%Y-%m-%d")))

#gvkey from data has leading 0's, not in firmfin : let's correct
firmfin$gvkey=sprintf("%06d", as.numeric(firmfin$gvkey)) #converts numeric to string (character)
firmfin$gvkey=factor(firmfin$gvkey)


Num_firmfin = firmfin %>% count(gvkey)


# Joins with what's common between the two datasets (link= gvkey and date). Adds CIK number (to exclude banking sectors )

Compiled_data= inner_join(data,firmfin, by=c("gvkey"="gvkey","datadate"="public_date"))


#Compiled_data2= inner_join(Compiled_data,CIK, by=c("gvkey"="CCMID"))





#data cleaning
# for example  :
boxplot(Compiled_data2$bm)


#detect outliers with Mahalanobis distance.
#----------------------------------------------------

anomalize(Compiled_data2,Compiled_data2$bm,  method = "iqr",  alpha = 0.05,  max_anoms = 0.2,  verbose = FALSE)

#---------------------------------------------------

# Compiled_data2 %<>%
#                 filter(!(abs(value - median(value)) > 2*sd(value)))




set.seed(100)

inTrain <- createDataPartition(  # stratified random split of the data
  y = Compiled_data2$defaultbool, ## the outcome data 
  p = .75,## The percentage of data in the training set
  list = FALSE
)










save(Compiled_data2, file= "./_DATABASE/[Clean] Compiled_Data.RData")


rm(data) # for memory efficiency
rm(firmfin) 
rm(CIK)
rm(Compiled_data)
rm(Var_desc)
rm(desc_data)
rm(conn)
