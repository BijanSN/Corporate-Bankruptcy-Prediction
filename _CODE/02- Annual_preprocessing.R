####################################
# Step 2 : Datasets Preprocessing  #
####################################

rm(list=ls())

#-------- Setup 

#library(plyr)
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

source("./_CODE/99 -Functions.R")

#-------- Loading Annual Financial dataset  -------------------------------------------------------------------------

load("./_DATABASE/[RAW] Annual_Data.RData") # 822'000+ annual observations of 45 financial items

#--------- Preprocessing Numerical financial dataset : Change classes & currency correction :---------------------

#--Change classes: 
annual_data %<>% mutate(GVKEY= as.factor(GVKEY)) %>%
                 mutate(FYEAR= as.factor(FYEAR)) %>%
                 mutate(DATADATE=as.factor(format(DATADATE, "%Y")))

# The Currency Translation Rate (CURRTR) “represents the rate of exchange used to translate foreign currency amounts into U.S. dollar amounts” (COMPUSTAT's definition)
# For example, a company has assets of 1million GBP in Compustat, and the currency translation rate is 1.5 then the USD amount is 1 million*1.5=1.5 million USD.

# Temporarily remove non-numeric columns(& unrelated numeric variables, ushc as share outstanding  for currency correction and apply the currency translation .
annual_numeric_only= subset(annual_data, select=c(-GVKEY,-DATADATE,-FYEAR,-CURRTR,-CSHO))*annual_data$CURRTR  # Currency correction

# Add the non-numerical columns again
annual_data=annual_numeric_only %<>%    mutate(GVKEY= annual_data$GVKEY) %>% 
                                        mutate(DATADATE= annual_data$DATADATE) %>% 
                                        mutate(FYEAR= annual_data$FYEAR) %>% 
                                        mutate(CSHO= annual_data$CSHO) %>% 
                                        relocate(DATADATE) %>%  # relocate the columns at the beginning of the dataset 
                                        relocate(FYEAR)    %>% 
                                        relocate(GVKEY) 

#----------------------- Preprocessing Financial dataset: Data cleaning  ----------------------------

#Missing map of the first 10'000 obs : 25 % NAs !
#vis_miss(annual_data[1:10000,]) 

#let's delete some NA-intensive variables :

annual_data %<>%  dplyr::select(-PI) %>%
                  dplyr::select(-OIADP) %>%
                  dplyr::select(-IB)

#Convert non-critical NA variables to 0 :
#annual_data %<>%  mutate(across(c("DVP","DVC","PRSTKC","IBC","DP"),~replace_na(.x, 0)))

#vis_miss(annual_data[1:10000,])

# imputed NA values for the remaining variables : (Warning: Very slow. ~5min with 5 VCPUs)
annual_data_no_NA = annual_data %>% 
                                  dplyr::group_by(GVKEY) %>%
                                  dplyr::mutate(across(everything(), na.aggregate)) %>% # replace NA with the mean of the firm's financials
                                  na.omit() # variable without group mean are erased

#Clear differences :
# vis_miss(annual_data[1:10000,])       # 822k observations
# vis_miss(annual_data_no_NA[1:10000,]) # 606k clean observations


save(annual_data_no_NA, file="./_DATABASE/[RAW] Annual_Data_no_NA.RData") #to prevent re-use of the previous function
#-------------
rm(list=ls())

load ("./_DATABASE/[RAW] Annual_Data_no_NA.RData")
annual_data=annual_data_no_NA 


#filter uninformative/trival cases (total assets <=0  for ex.). Useful for log transformation later on
annual_data %<>% filter(across(c("AT","ACT","CAPX","CHE","CSHO","LT","SALE","LCT","PRCC_C","PRCC_C"),~.x>0)) 

save(annual_data, file="./_DATABASE/[RAW] Annual_Data_no_NA.RData")

#--------------------  Filtering financial data  ------------------------------

rm(list=ls())
load("./_DATABASE/[RAW] Annual_Data_no_NA.RData")

annual_data %<>%  filter(!AT>100000,!ACT>15000,!CAPX>4000,!CHE>10000, #remove if [broad condition]
                              !COGS>40000,!DP>6000,!DVP<0,!EPSPX<= -5000,!LCT>15000,
                              !LT>75000,!NI>10000,!SALE>40000,!DLC>7500,!EPSPX<=-10000,
                              !DVT>2000,!DVT<0,!EBIT<=-4500,!DLTT>30000,!ICAPT>80000,!ICAPT<=-20000,!DVP<0)#!AO>20000

save(annual_data, file="./_DATABASE/[RAW] Annual_Data_no_NA.RData")

#--------- Preprocessing Numerical financial dataset : Scaling ---------------------

# SKIPED

rm(list=ls())
load("./_DATABASE/[RAW] Annual_Data_no_NA.RData")
financial_data=annual_data
# Do visualisation of every variables :

summary(financial_data)

# Apply regular standardisation ((X-mean)/sd) on all variables, for comparability purposes : /!\ interpretation of the models 
# "Standardization does not affect logistic regression, decision trees and other ensemble techniques such as random forest and gradient boosting." 

financial_data[c(-(1:3))]<- scale(financial_data[c(-(1:3))])
scaled_financial_data = financial_data
num= scaled_financial_data[c(-(1:3))]

cor=cor(num, use = "complete.obs", method = "pearson")

heatmap(x = cor, symm = TRUE) # Multiicolinearity ! Variable selection when modeling.

res1 <- cor.mtest(num, conf.level = .95)
res2 <- cor.mtest(num, conf.level = .99)

 #corrplot::corrplot(cor, p.mat = res1$p, insig = "blank",tl.col = "black",order = "hclust")

#only on potentially negative ones :
# financial_data[c("Z_NI","Z_OIBDP","Z_OIADP","Z_PI",
#                  "Z_RE","Z_EPSPX","Z_WCAP","Z_CEQ",
#                  "Z_IBC","Z_EBIT","Z_EBITDA","Z_SEQ",
#                  "Z_IB","Z_ICAPT")]<- scale(financial_data[c("NI","OIBDP","OIADP","PI",
#                                                          "RE","EPSPX","WCAP","CEQ","IBC","EBIT",
#                                                          "EBITDA","SEQ","IB","ICAPT")])

save(scaled_financial_data, annual_data, file= "./_DATABASE/[CLEAN] scaled_filtered_Financial_Data_1.RData")


#---------- Preprocessing Ratings dataset (From Nuvolos)  --------

#-- Load raw ratings dataset 
load("./_DATABASE/[RAW] Ratings_data.RData")

ratings_data %<>%                                                   # pipe opperator "%<>%" applies the right-hand-side functions and assigns to L.H.S.
            na.omit() %>%                                                                     # x %>% f(y) =  f(x,y). removes uncomplete data
            dplyr::rename(ratings=SPLTICRM) %>%                                               # rename to something clearer
            filter(!ratings %in% c("N.M.","SD","Suspended")) %>%                              # remove unusable ratings
            mutate(GVKEY=factor(GVKEY)) %>%
            dplyr::group_by(GVKEY) %>%
            mutate(DATADATE=factor(format(DATADATE, "%Y")),                                           # convert from 'numeric' to 'Date' type. Drop days and months.
                   ratings= factor(ratings,levels=c("AAA","AA+","AA", "AA-",
                                                    "A+","A","A-","BBB+","BBB",
                                                    "BBB-", "BB+","BB","BB-",
                                                    "B+","B-","B","CCC+","CCC",
                                                    "CCC-","CC","C","D"))) %>%                # convert from 'character' to 'factor' type, in quality's descending order
            droplevels() 
  # defaultdate=dplyr::case_when(defaultfuture==1 ~min(DATADATE),              # create a new column with the default date, if applicable
  #                              defaultfuture==0 ~as.Date('2100-12-12'))) %>% # can't put 'NA' directly : I use a fake date to convert afterwards.


# Limit to one rating observation per year
ratings_data %<>%
                 group_by(GVKEY,DATADATE) %>%
                 distinct(DATADATE, .keep_all = T) %>% 
                 arrange(GVKEY,DATADATE)


# Create default dummies: 

ratings_data %<>%
                group_by(GVKEY) %>% 
                mutate(defaultbool= ifelse(ratings=='D',1,0),                          # create a new column "default_bool" =1 if the compagny defaults at time t=i
                       defaultfuture = max(defaultbool),                               # create bool if the company will default in the future.
                       oneyeardefaultpred = ifelse(lead(defaultbool)==1,1,0),          # create bool =1 if defaults the next year ( add "& ratings !="D" " ?)
                       oneyeardefaultpred = replace_na(oneyeardefaultpred, 0))         # if NA : bool= 0
  
save(ratings_data, file= "./_DATABASE/[CLEAN] Ratings_data.RData")












# --------------------------------------------------------------------------------
#------------- Merging financial & ratings datasets ------------------------------

rm(list=ls())

#--loading ratings & financial datasets 

load("./_DATABASE/[CLEAN] Ratings_data.RData")
load("./_DATABASE/[RAW] Annual_Data_no_NA.RData") # FILTERED , UNSCALED


#load("./_DATABASE/[CLEAN] Financial_Data_1.RData") 
#load("./_DATABASE/[CLEAN] scaled_filtered_Financial_Data_1.RData")
financial_data= annual_data
#financial_data=scaled_financial_data

#scaled_financial_data = scaled
#financial_data = unscaled

ratings_D= ratings_data %>% filter(ratings=='D')

glimpse(financial_data)
glimpse(ratings_data)

# ratings D = proxy for defaulted companies

DEFAULT_RATINGS_TO_PREDICT= inner_join(financial_data,ratings_D, by=c("GVKEY"="GVKEY","DATADATE"="DATADATE")) #scaled_
#530 observations to predict...

#collect unique gvkeys:
GVKEY_TO_PREDICT= DEFAULT_RATINGS_TO_PREDICT %>% group_by(GVKEY) %>% count() # from 180 unique firms
givenkeys= subset(GVKEY_TO_PREDICT,select = "GVKEY")

#all history of the defaulted gvkeys: 3086 obs. 534 D. //1998 obs
ONLY_DEFAULTED_FIRMS_FINANCIALS= left_join(givenkeys,financial_data, by="GVKEY") %>%            # Take only financials from GVKEY that defaulted
                                 inner_join(.,ratings_data, by=c("GVKEY"="GVKEY","DATADATE"="DATADATE")) %>%      # Add Ratings
                                 group_by(GVKEY,DATADATE) %>% 
                                 distinct(DATADATE, .keep_all = T) %>% 
                                 arrange(GVKEY,DATADATE)


ALL_FIRMS_RATINGS_FINANCIALS= inner_join(financial_data,ratings_data, by=c("GVKEY"="GVKEY","DATADATE"="DATADATE"))
#OUT OF 66K obs.

save(DEFAULT_RATINGS_TO_PREDICT,ONLY_DEFAULTED_FIRMS_FINANCIALS,ALL_FIRMS_RATINGS_FINANCIALS,file="./_DATABASE/[CLEAN] Final_data.RData") 
# unscaled but filtrered

#------------- Preprocessing : outliers deletion from non-bankrupt companies ---------------------
# rm(list=ls())
# load("./_DATABASE/[CLEAN] Final_data.RData")


  # col_names= colnames(ONLY_DEFAULTED_FIRMS_FINANCIALS[c(-(40:42))])
  # l1 <- vector("list", length(ONLY_DEFAULTED_FIRMS_FINANCIALS[c(-(40:42))]))
  # for(i in seq_along(col_names)){
  #   variable = col_names[i]
  #   plt=ggplot(ONLY_DEFAULTED_FIRMS_FINANCIALS[c(-(40:42))], aes_string(x = variable)) + 
  #     geom_boxplot() + 
  #     facet_wrap(ONLY_DEFAULTED_FIRMS_FINANCIALS$defaultbool, scales = 'free_y')+
  #     ggtitle(paste("Distribution of",variable, "given default"))
  #   print(plt)
  #     }
  # 

# print_plot_coeffs_facet_default(ONLY_DEFAULTED_FIRMS_FINANCIALS) #visualisation for broad oulier detection :



#put INVT >0 !
#INVT interesting ! RE too
#filter(!BKVLPS<0))                                              
#filter(!WCAO<-7500))                                               
#MHMMmm SALE
#filter-out 2 defauts.  |NI< -20000)
# CAPXshouldnt. honestly too much, but helps cutting number of 0's
# filter(!OIBDP=(OIBDP>6000|OIBDP< -3000)) #filter-out 2 defauts
#PPENT UNTOUCHED,rvent too


# 
# save(DATA,file="./_DATABASE/[CLEAN] Final_data.RData")
#------------------------------------------------------------------------------------------
#---------------- Feature engineering : Create financial ratios / variables for predicting default  : -------------------------------------------

rm(list=ls())
load("./_DATABASE/[CLEAN] Final_data.RData")



#Broadly Remove outliers from unscaled variables : (only filter errors such as -15000 EPS. Very conservative filter)
#DATA= ONLY_DEFAULTED_FIRMS_FINANCIALS  %>%  filter(!AT>100000,!ACT>15000,!CAPX>4000,!CHE>10000, #remove if [broad condition]
                                                   #!COGS>40000,!DP>6000,!DVP<0,!EPSPX<= -5000,!LCT>15000,
                                                   # !LT>75000,!NI>10000,!SALE>40000,!DLC>7500,!EPSPX<=-10000,                                                   #!DVT>2000,!DVT<0,!EBIT<=-4500,!DLTT>30000,!ICAPT>80000,!ICAPT<=-20000,!DVP<0)#!AO>20000
#summary(DATA)
#boxplot(DATA$IBC)
# Create Log variables (control) & normalise-it: 
# DATA %<>% mutate(across(c("AT", "ACT","CHE","CAPX","COGS","INVT","LCT","LT","SALE","SEQ","XOPR", "XINT","DLTT","DVT","DLC"),
#                                                  ~log(1+.x),.names = "LOG_{.col}")) 


#plot_coeffs(DATA) #HOMEMADE FUNCTION 
#summary(DATA)


# capture firm size factor through "flat" numbers (not ratios), applied log.
# 
# DATA %<>% dplyr::mutate(across(c("log_1+AT", "log_1+ACT","log_1+CHE","log_1+CAPX","log_1+COGS","log_1+INVT","log_1+LCT",
#                           "log_1+LT","log_1+SALE", "log_1+XOPR", "log_1+XINT","log_1+DLTT","log_1+DVT","log_1+DLC"),
#                           ~nor(.x) , .names = "norm_{.col}"))



# logb(x+a)
# a=max(0,−min(x)+ϵ)

# 
# boxplot(DATA$CSH_FLOW)
# boxplot(DATA$TEST)
# boxplot(bestNormalize(DATA$CSH_FLOW))


# Buiding variable candidates for default prediction :
DATA=ONLY_DEFAULTED_FIRMS_FINANCIALS
DATA %<>%
   mutate( # Liquidity / Capital Structure
          MKVALT=CSHO*PRCC_C, # Market value
          #LOG_MKVALT=log(CSHO*PRCC_C),
          
          WC_TA= WCAP/AT,
          #LOG_WC_TA= log(WCAP/AT),
          
          CSH_FLOW= ((IBC+DP)/AT), #Cash Flow
          #LOG_CSH_HOLD=log(CHE/AT), 
          CSH_FLOW_CL= CSH_FLOW/LCT, # Cash Flow/Current Liabilities 
          CSH_FLOW_LT= CSH_FLOW/LT, # Cash Flow/Total Debt   

          #LOG_CA_CL=log(ACT/LCT),   # Current ratio : Current Assets/Current Liabilities
          #LOG_CHE_CL= log(CHE/LCT), #log(.)# Cash+E/Current Liabilities   
          #LOG_CHE_AT= log(CHE/AT), # Cash+E/Total Assets
          #LOG_CL_E=log(LCT/MKVALT),     # Current Liabilities/Equity 
          #LOG_CA_TD= log(ACT/LT),   # Current Assets/Total Debt 
          
          
          #LOG_MKT_TA=log(MKVALT/AT), 
          MTB=MKVALT/BKVLPS,#Market to Book Ratio : not sure if works, check corr with MTTA
          ST_TD=LCT/(DLTT+DLC), # Short term debt /Book value of Debt 
          LVRG=(DLTT+DLC)/SEQ, #Leverage
          
          # # Size ("flat" numebers)
          # LOG_TA=log(AT),
          # LOG_SAL=log(SALE),
          # LOG_CAPX=log(CAPX),
          # # new :
          # LOG_XOPR=log(XOPR),
          # LOG_CEQ=log(CEQ),
          # LOG_XINT=log(XINT),
         
          # Growth:

          CSH_CAPX= CSH_FLOW/CAPX,
          CH_OP_MAR= OIBDP/SALE, # change in operating margins
          
          #Profitability
         
          EPS=NI/CSHO,
          ROA=NI/AT,
          ROE=NI/(CSHO*PRCC_C),
          #LOG_SAL_TA=log(SALE/AT),
          EBIT_TA= EBIT/AT,
          EBIT_SAL= EBIT/SALE,
          # EBIT_INVCAP=EBIT/ICAPT, # EBIT/Invested Capital ! divided by 0
          NI_TA= NI/AT,           # NI/Total Assets 
          SAL_TA=SALE/AT,
          #Others: 
          #COC=XINT/DLC,
          #LOG_COC=log(XINT/DLC),  #Cost of Capital (Interest expenses/total debt), after tax
          RE_AT=RE/AT,            #REQ Retained earnings/total assets
)

#DATA %<>% filter(AT<5)

#boxplot(log(DATA$AT))

#qplot(DATA$ACT)
#DATA %<>% na.omit()
#vis_miss(DATA)
#summary(DATA)

#final cleaning :
#DATA   %<>%  filter(!EBIT_INVCAP>100,!EBIT_TA>5,!ROE<=-10000,!EPS<=10000,!CH_OP_MAR<=-100)# ,!CSH_CAPX>7,!ST_TD>5000
                    

#transform higly skewed variables !


boxplot(DATA$CSH_FLOW)
boxplot(DATA$WC_TA)

# cant log it !

scale_me <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
#=> scale variable who are neg.


#colnames(DATA)
DATA=as_tibble(DATA)
SCALED_DATA= DATA  %>%  mutate(across(AT:ICAPT,scale_me))
#SCALED_DATA= DATA  %>%  mutate(across(where(is.numeric)),scale_me))
  
# scale every variables but prcc, csho - ! Interpretation

SCALED_DATA2= DATA  %>%  mutate(across(c("AT","ACT","AO", "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                                        "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                                        "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                                        "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA",
                                        "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , 
                                        "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"SAL_TA","EBIT_SAL" ,
                                        "NI_TA" , "RE_AT"),scale_me))  #.names = "scaled_{.col}")))"COC" "ST_TD", "LVRG",

vis_miss(SCALED_DATA2)


# -logs, because NAs if <0
summary(SCALED_DATA)
boxplot(SCALED_DATA$EPS) #scaling doesn't change distribution

#changes interpretation . increase in percentile : +x % 
library(heatmaply)
NORM_DATA= DATA  %>%  mutate(across(c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                                      "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                                      "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                                      "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA","LVRG",
                                     "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , "ST_TD",
                                     "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"SAL_TA","EBIT_SAL" ,
                                     "NI_TA" , "RE_AT"),percentize))  #.names = "scaled_{.col}"))"COC")



#print_plot_coeffs_facet_default(NORM_DATA[,c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
#                                             "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
#                                             "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
#                                             "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA","LVRG",
#                                             "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , "ST_TD"]))




DATA %>% dplyr::select(AT:ICAPT) %>% cor() %>%heatmaply() #same
SCALED_DATA %>% dplyr::select(AT:ICAPT) %>% cor() %>% heatmaply()#same
NORM_DATA %>% dplyr::select(AT:ICAPT) %>% cor() %>% heatmaply()#changes slightly.


DATA %>% select(c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                  "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                  "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                  "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA",
                  "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , 
                  "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"EBIT_SAL" ,
                  "EBIT_INVCAP" ,"NI_TA", "RE_AT")) %>% cor() %>% heatmaply() #same"COC" "ST_TD","LVRG",



SCALED_DATA %>%select(c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                        "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                        "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                        "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA","LVRG",
                        "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , "ST_TD",
                        "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"EBIT_SAL" ,
                        "EBIT_INVCAP" ,"NI_TA" , "COC" , "RE_AT")) %>% cor() %>% heatmaply() 


NORM_DATA %>% select(c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                       "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                       "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                       "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA","LVRG",
                       "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , "ST_TD",
                       "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"EBIT_SAL" ,
                       "EBIT_INVCAP" ,"NI_TA" , "COC" , "RE_AT")) %>% cor() %>% heatmaply() 


summary(NORM_DATA) # log or not : doesnt matter anymore ! same percentile



#----------
DATA %>% ggplot(., aes(x=DATADATE)) + 
         geom_bar(stat="count")+
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
         labs(title="Financial data availability per year",
              x ="Years", y = "Counts")

# dev.copy(png,'./_FIGURES/financial_data_availability.png')
# dev.off()
#----------
#quick visu :

#source("./_CODE/99 -Functions.R")
# print_plot_coeffs_facet_default(DATA[1:76])
# 
# print_plot_coeffs_facet_default_hist(DATA[39:76])


col_names= colnames(DATA)
l1 <- vector("list", length(DATA))

for(i in seq_along(col_names)){
  variable = col_names[i]
plt=ggplot(DATA, aes_string(x = variable)) + 
  geom_boxplot() + 
  facet_wrap(DATA$defaultbool, scales = 'fixed')+ #instead of free_y
  ggtitle(paste("Distribution of",variable, "given default"))+
  coord_flip()
print(plt)
}


# delete outliers:

DATA  %<>%  filter(!SAL_TA>10,!EBIT_SAL<= -100,!EBIT_TA>2.5,
                   !NI_TA>5,!LVRG>3000)#!INV_SA>4!COGS_INV>1000,!COGS_SAL>40,!NI_E<= -10000,!NI_SAL<= -100,!EBIT_INVCAP>25,!EBIT_INVCAP<= -25,
#!WC_SAL>50, !QA_CL<= -25,!CH_AT>3,!CH_CL>100,

NORM_DATA= DATA  %>%  mutate(across(c("AT","ACT","AO" , "CAPX" , "CHE","COGS" ,"DP","EPSPX","INVT","LCT",
                                      "LT","NI" ,"OIBDP","PPENT","RE","REVT" ,"SALE","WCAP","XOPR","CEQ",
                                      "IBC" ,"XINT", "DLC","DVT","EBIT","EBITDA","BKVLPS" ,"DLTT" ,"SEQ",
                                      "DVP","DVC","PRSTKC", "ICAPT" ,"MKVALT" , "WC_TA","LVRG",
                                      "CSH_FLOW","CSH_FLOW_CL", "CSH_FLOW_LT" ,"MTB" , "ST_TD",
                                      "CSH_CAPX" ,"CH_OP_MAR" ,"EPS" , "ROA" , "ROE", "EBIT_TA" ,"SAL_TA","EBIT_SAL" ,
                                      "NI_TA" , "RE_AT"),percentize))  #.names = "scaled_{.col}"))"COC")

save(DATA,NORM_DATA, file="./_DATABASE/[FINAL] MODEL_DATA.RData")
