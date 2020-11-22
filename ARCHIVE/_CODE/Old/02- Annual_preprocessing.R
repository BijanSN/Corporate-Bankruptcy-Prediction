####################################
# Step 2 : Preprocessing  datasets #
####################################

rm(list=ls())

#--------Setup------------------------------------------------
#library(plyr)
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

#-------------------------------------------------------------
#-------- Loading Annual Financial dataset 

load("./_DATABASE/[RAW] Annual_Data.RData")

#--------- Preprocessing  

annual_data %<>%
  group_by(GVKEY,DATADATE) %>% 
  filter(!str_detect(SICH, "^6"))            %>%                   # non financial firms only. SICH= Standard Industrial Classification (SIC)
  filter_at(vars(ACT,AO,CH), any_vars(!is.na(.)))                  # remove if these variables are not (co-)present.

# vis_miss(annual_data[1:10000,])
# vis_miss(annual_data[10000:29000,])

# no patterns on missing values given time nor givenkeys.


# Currency correction :
annual_data %<>% select(-SICH)
annual_data %<>% select(-CURNCD) # Currency code
annual_data[,-c(1:3)]= annual_data[,-c(1:3,)]*annual_data$CURRTR 
annual_data %<>% select(-CURRTR)


# Create ratios : 
annual_data %<>%
  #Book Value of Equity
  mutate(BV= PRCC_C*CSHO,
         
         #Cash Flow
         CSH_FLOW= (IBC+DP)/AT,
         
         #Cash Holding
         CSH_HOLD=CHE/AT,
         
         #Cost of Capital (Interest expenses/total debt), after tax
         COC=XINT/DLC,
         
         #Earnings per Share
         EPS=NI/CSHO,
         
         #Leverage
         LVRG=(DLTT+DLC)/SEQ,
         
         #Market Value
         MKVALT=CSHO*PRCC_C,
         
         #Market to Book Ratio
         MTB=MKVALT/BKVLPS,
         
         #Payout Ratio
         PAYOUT_R=(DVP + DVC + PRSTKC)/IB,
         
         #ROA
         ROA=NI/AT,
         
         #ROE
         ROE=NI/CSHO*PRCC_C,
         
         #------Normalisation of variables (generaly over assets or liabilities)
         
         #Working capital/total assets 
         WC_TA= WCAP/AT,
         
         #REQ Retained earnings/total assets
         RE_ATQ= RE/AT,
         
         #Current Assets/Current Liabilities 
         CA_CL=ACT/LCT,
         
         # Cash/Current Liabilities             
         CH_CL= CH/LCT,
         
         # Cash/Total Assets
         CH_AT= CH/AT,
         
         # Cash Flow/Current Liabilities 
         CSH_FLOW_CL= CSH_FLOW/LCT,
         
         # Cash Flow/Total Debt 
         CSH_FLOW_LT= CSH_FLOW/LT,         
         
         # Current Liabilities/Equity 
         CL_E=LCT/BV,          
         
         # Current Assets/Total Debt 
         CA_TD= ACT/LT,
         
         # Quick Assets/Current Liabilities 
         # Quick Assets = Current Assets – Inventories
         # INVT Inventories - Total
         QA_CL=(ACT-INVT)/LCT,        
         
         # Quick Assets/Inventories 
         QA_INV=(ACT-INVT)/INVT,
         
         # Working Capital/Sales
         #"The term sales means revenues without VAT."
         WC_SAL=WCAP/SALE,
         
         # Inventories/Sales 
         INV_SA=INVT/SALE,  
         
         # Accounts Receivable/Sales 
         #AR_SAL=ARTFS/SALE,  
         
         # AR/Inventories
         #AR_INV= ARTFS/INVT,
         
         # COGs/Inventories 
         COGS_INV= COGS/INVT,
         
         # COGs/Sales 
         COGS_SAL= COGS/SALE,
         
         # NI/Total Assets 
         NI_TA= NI/AT,
         
         # NI/Equity 
         NI_E=NI/BV,         
         
         # NI/Sales 
         NI_SAL=NI/SALE,
         
         # EBIT/Invested Capital
         EBIT_INVCAP=EBIT/ICAPT,
         
         # EBIT/Total Assets 
         EBIT_TA= EBIT/AT,
         
         # EBIT/Sales 
         EBIT_SAL= EBIT/SALE,
         # Sales/Total Assets
         SAL_TA=SALE/AT)

save(annual_data, file= "./_DATABASE/[RAW] Annual_Data2.RData")

# -------------- Cleaning & Visualisation -----------------
load("./_DATABASE/[RAW] Annual_Data2.RData")

# glimpse(annual_data)
# vis_miss(annual_data[1:10000,])

annual_data %<>% na.omit()
#vis_miss(annual_data[1:10000,])



# companies_occurences= annual_data %>% 
#                                       group_by(GVKEY) %>% 
#                                       select(GVKEY) %>% 
#                                       summarise(counts = n()) #9860 companies
# 
# 
# ggplot(companies_occurences, aes(x=counts)) +
#   geom_histogram()

# mean(companies_occurences$counts) # average 7.6 years of data
# median(companies_occurences$counts) # median = 5years


# variables cleanings: /!\ division by 0 : ratios = INF
# range of variables?

# Rule of thumb to select the bandwidth: h = σ*T−(1/5) where σˆ is empirical standard deviation of the data. binwidth = sd(annual_data$EBIT)*(NROW(annual_data$EBIT)^(-1/5)) or
#Square-root choice : k= sqrt(n): (binwidth =sqrt(NROW(annual_data$EBIT)))


summary(annual_data)

#  FYEAR
#Min.   :1984
#Max.   :2019

#remove "outliers" (to improve/check if not too many defaults from outliers)
annual_data %<>% filter(AT<quantile(AT,0.90) & AT>quantile(AT,0.1)) %>% 
                 filter(CH<quantile(CH,0.90) & CH>quantile(CH,0.1))

# Standardization
summary(annual_data)

# preproc1 <- preProcess(test, method=c("center", "scale")) 
# test2 <- predict(preproc1,test)
# summary(test2)

normalize <- function(x, na.rm = TRUE) (x - mean(x, na.rm = T))/sd(x, na.rm = T) # Changes scale. Mean = 0 sd = 1.


# Standardization
norm_data= annual_data %>% 
                          mutate_at(vars(AT:SAL_TA), normalize)  # values = number of std from the mean

#boxplot(test$EBIT)

# ---------------------------

t1= annual_data %>%  group_by(GVKEY) %>% 
                 select(GVKEY,EBIT) %>% 
                 summarise(min= min(EBIT)) %>% 
                 summarise(max= max(EBIT)) %>% 
                 
  
  
boxplot(annual_data$COGS)

annual_data %>%
  ggplot(., aes(x="",y=EBIT)) + 
  geom_boxplot(outlier.colour=NA)


annual_data %>%
               filter(EBIT<10000) %>% 
               ggplot(., aes(EBIT)) +
               scale_x_continouous(limits=c(0, 1000)) +
               geom_histogram()
              

annual_data$EBIT

ggplot(annual_data, aes(EBIT)) + 
geom_histogram()


ggplot(annual_data, aes(x = EBIT)) +
  geom_bar()


filter(annual_data$EBIT<25000)

 
# annual_data %>%
#                group_by(GVKEY) %>%
#                summarize(mean_weight = mean(weight))





save(annual_data, file= "./_DATABASE/[RAW] Annual_Data2.RData")





















