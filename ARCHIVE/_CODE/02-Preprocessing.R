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



#Currency correction :

annual_data %<>% select(-CURNCD) # Currency code
annual_data= annual_data[,-c(1:3)]*annual_data$CURRTR 
annual_data %<>% select(-CURRTR)

# create ratios : 
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
                       AR_SAL=ARTFS/SALE,  
                       
                       # AR/Inventories
                       AR_INV= ARTFS/INVT,
                       
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
                       

glimpse(annual_data)




save(annual_data, file= "./_DATABASE/[RAW] Annual_Data2.RData")

                       
#--------Financial dataset : visualisation and preprocessing : 

load("./_DATABASE/[RAW] Financial_Data.RData")

#-------------------------------------------------------------

financial_data %<>%
  dplyr::arrange(FYEARQ)

#vis_miss(financial_data[1:10000,]) # major data missing before row 2'700 = fiscal year 1971. 26.7% missing data

#CURUSCNQ = NA for non-canadian firms (exchange rate for 1 USD)
#firms with non-US childs.

financial_data %<>% mutate(CURUSCNQ = replace_na(CURUSCNQ, 1))

financial_data %<>%
  filter(FYEARQ>1975) %>% # after change : less missing values !
  group_by(GVKEY) 

#vis_miss(financial_data[1:10000,])
#heatmap(lapply(na.omit(financial_data, as.numeric))


financial_data %<>% 
  filter_at(vars(ACTQ,AOQ,ATQ,CHEQ), any_vars(!is.na(.))) # 6.8%. 

# add no NA for AOQ ?
# financial_data %<>% 
#   filter_at(vars(ACTQ), any_vars(!is.na(.))) # 6.8%. 


#vis_miss(financial_data[1:10000,])

# vis_miss(financial_data[10000:20000,]), 'warn_large_data'= F)
# vis_miss(financial_data[20000:30000,])
# vis_miss(financial_data[30000:40115,])


glimpse(financial_data)

quarter1=lubridate::quarter(financial_data$DATADATE)

yq1 <- as.yearqtr(financial_data$DATADATE, format = "%Y-%quarter1")
year_quarter1= format(yq1, format = "%Y-0%q")

financial_data$financial_year_quarter= year_quarter1
financial_data$GVKEY=factor(financial_data$GVKEY)

#------test----------------------
financial_data %<>% na.omit()
# ----- Currency correction -----

col_names=colnames(financial_data)
print(col_names)

# financial_data2 = financial_data %>% 
#                                  select(-DATADATE:-FQTR) %>% 
#                                  select(-CURRTRQ:-CURUSCNQ) %>% 
#                                  select(-financial_year_quarter)


#------- filter v2 test ---------

financial_data %<>%
  filter_at(vars(ACTQ), any_vars(!is.na(.))) # if Na on Total asset : omit the observation. see if it helps the models or not.


#-------------- Financial Covariates ----------------

#not included :
#GVKEY Global Company Key
#TIC
#DATADATE Date
#FYEARQ Fiscal Year
#FQTR Fiscal Quarter

attach(financial_data) # all data

## financials items :

#ACTQ Current Assets - Total
#AOQ Assets - Other - Total
#ATQ Assets - Total
#CHEQ Cash and Short-Term Investments
#EPSPXQ Earnings Per Share (Basic) - Excluding Extraordinary items
#INTACCQ Interest Accrued : too much NAs
#INVTQ Inventories - Total
#LCTQ Current Liabilities - Total
#LLTQ Long-Term Liabilities (Total) : too much NAs
#LTQ Liabilities - Total
#NIMQ Net Interest Margin: too much NAs
#NIQ Net Income (Loss)
#OIADPQ Operating Income After Depreciation
#PIQ Pretax Income
#PLLQ Provision for Loan/Asset Losses: : too much NAs
#PPENTQ Property Plant and Equipment - Total (Net)
#RECTQ Receivables - Total
#RETQ Total RE Property : 100% NAs
#REVTQ Revenue - Total
#SALEQ Sales/Turnover (Net)
#UINVQ Inventories
#UGIQ Gross Income (Income Before Interest Charges)
#UDOLTQ Debt (Other Long-Term) : too much NAs
#SPCEQ S&P Core Earnings : too much NAs
#ULCOQ Current Liabilities - Other <: too mmuch NAs
#DPY Depreciation and Amortization - Total                    try DPCY instead.
#CAPXY Capital Expenditures
#AQCY Acquisitions
#COGSY Cost of Goods Sold
#ESUBCY Equity in Net Loss/Earnings (C/F)
#INTPNY Interest Paid - Net : too much NAs
#NIMY Net Interest Margin
#NIY Net Income (Loss)
#PIY Pretax Income
#PLLY Provision for Loan/Asset Losses
#REVTY Revenue - Total
#SALEY Sales/Turnover (Net)
#MKVALTQ Market Value - Total : too much NAs
#SPCEY S&P Core Earnings: too much NAs
#CIK CIK Number
#DVPSPQ Dividends per Share - Pay Date - Quarter
#UGIQ Gross Income






# ALtman Z score :

financial_data %<>%
               mutate(WCAPQ_ATQ= WCAPQ/ATQ,

                      #REQ Retained earnings (Net income-dividends) # to check with Net Income -dividends manually !
                      REQ_ATQ= REQ/ATQ,
      
                      # EBIT (Bottom up approach)
                      # Revenue – Cost of Goods Sold – Operating Expenses
                      # REVTQ Revenue - Total
                      # COGSQ  Cost of Goods Sold
                      # XOPRQ Operating Expense- Total
                      
                      EBIT=REVTQ-COGSQ-XOPRQ,
      
      #EBIT : top down
      # Net Income + Interest + Taxes
      # NIY + 
      #XINTQ Interest and Related Expense- Total
      #TXTQ Income Taxes - Total
                      EBIT2= NIQ+ 
      
      
      
      
                      #Current Assets/Current Liabilities 
                      #ACTQ Current Assets - Total / 
                      #LCTQ Current Liabilities - Total
                      CA_CL=ACTQ/LCTQ,
      
                      # Cash/Current Liabilities             : => find cash only
                      #CHEQ Cash and Short-Term Investments
                      #LCTQ Current Liabilities - Total
                      CH_CL= CHEQ/LCTQ,
      
                      # Cash/Total Assets
                      #CHQ Cash
                      #ATQ Assets - Total
#                      CH_ATQ= CHQ/ATQ,

# Cash Flow/Current Liabilities 
#free cash flow :  Cash Flows from Operating Activities + intrests expense - tax shield on interest expanse -CAPEX


# Cash Flow/Total Debt 


# Current Liabilities/Equity 
#Equity (book value)

                      
                      # Current Assets/Total Debt 
                      #ACTQ Current Assets - Total
                      #LTQ Liabilities - Total
                      CA_TD= ACTQ/LTQ,


# Quick Assets/Current Liabilities 
#CHEQ Cash and Short-Term Investments. (quick assets ? )
#LCTQ Current Liabilities - Total


# Quick Assets/Inventories 
#CHEQ Cash and Short-Term Investments. (quick assets ? )
#UINVQ Inventories

                      
                      
                      # Working Capital/Total Assets
                      WC_TA= WCAPQ/ATQ,
                      
                      # Working Capital/Sales
                      #"The term is often just referred to as sales or net sales, which means revenues without VAT."
                      WC_SAL=WCAPQ/SALEQ,

# Inventories/Sales 

# Accounts Receivable/Sales 

# AR/Inventories: account receivables to find in income statements.

                      # COGs/Inventories 
#                      CO_INV= COGSQ/UINVQ,
                      
# COGs/Sales 
                      
                      
                      # NI/Total Assets 
                      NIQ_TA= NIQ/ATQ,
                        
# NI/Equity 


# NI/Sales 
                      
                      # Retained Earnings/Total Assets
                      RE_TA= REQ/ATQ,
                      
# EBIT/Invested Capital
                      
                      # EBIT/Total Assets 
                      EBIT_TA=EBIT/ATQ)

# EBIT/Sales 

# Sales/Total Assets


# forum :

# Market Value :
# CEQQ: Common/Ordinary Equity - Total
# PRCCQ: Price Close - Quarter
# CSHOQ: Common Shares Outstanding

# mtb = prccq * cshoq / ceqq;
# 
# 
# total_debt = dlc + dltt
# lt_debt    = dd1 + dltt
# ltd_ratio  = lt_debt/total_debt

# add 'q'



test_data = financial_data%>%  select(EBIT,REVTQ,COGSQ,XOPRQ)









#---------test -----------------
# financial_data %<>% group_by(GVKEY)
# currency_corrected = financial_data %>%
#                                     mutate_at(vars(WCAPQ_ATQ:EBIT_TA), .funs = funs(. * CURRTRQ))

ratios_data = financial_data %>% 
                              select(GVKEY,financial_year_quarter,WCAPQ_ATQ:EBIT_TA) 
                         

ratios_data= ratios_data[,-1:-2]*CURRTRQ #currency correction

ratios_data%<>% mutate(GVKEY= financial_data$GVKEY)  %>% 
                mutate(financial_year_quarter= financial_data$financial_year_quarter)

ratios_data %<>% select(GVKEY,financial_year_quarter,WCAPQ_ATQ:EBIT_TA)

ratios_data %<>% group_by(GVKEY)%>%
                 arrange(GVKEY,financial_year_quarter) 
# relocate(GVKEY)
#-----------------------------

save(financial_data, file= "./_DATABASE/[CLEAN] Financial_Data.RData")
save(ratios_data, file= "./_DATABASE/[CLEAN] Ratio_Data.RData")
rm(financial_data)
rm(quarter1)

#--------------------------------------------------------------------------------
#--------Delisting dataset : Preprocessing :

load("./_DATABASE/[RAW] Delist_data_raw.RData")
load("./_DATABASE/[RAW] Link_data.RData")

#-------------------------------------------

delist_data_clean = delist_data_raw %>%
                                      dplyr::rename(Exchange=HEXCD)     %>%
                                      dplyr::rename(SIC=HSICCD)         %>%
                                      dplyr::rename(Delist_date=DLSTDT) %>%
                                      dplyr::rename(Delist_code=DLSTCD) %>% 
                                      mutate(SIC=factor(SIC))           %>% 
                                      mutate(Delist_code= factor(Delist_code))
# filter(SIC <6000 & SIC >6999)
# defaultbool= ifelse(ratings=='D',1,0),                                     # create a new column "default_bool" =1 if the compagny defaults at time t=i
# defaultfuture = max(defaultbool),                                          # create bool if the company will default in the future.
# oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0),                       # create bool =1 if defaults next year ( add "& ratings !="D" ")

# temporary load plyr package for mapvalues function
# library(plyr)
# delist_data_clean$Exchange=plyr::mapvalues(delist_data_clean$Exchange, from = c("1","2","3","4","5"), to =c("NYSE", "NYSE MKT", "NASDAQ", "Arca" ,"Bats"))
# detach(package:plyr) # to avoid packages conficts#delist date to the end of month ( to match the other dataset) :


delist_data_clean$Delist_date <- ceiling_date(delist_data_clean$Delist_date, "month") - days(1)
#delist_data_clean$Delist_date <-format(delist_data_clean$Delist_date, "%Y-%m")
#delist_data_clean$Delist_date <-format(delist_data_clean$Delist_date, "%Y-0%q")


# gvkey= CCMID Compustat’s permanent identifier
# LPERMCO = CRSP PERMCO link

# replace CRSP's PERMCO with COMPUSTAT's CCMID/gvkey :
delist_data_clean = inner_join(delist_data_clean,link_data, by=c("PERMCO"="LPERMCO"))

delist_data_clean %<>% select(CCMID,Exchange:Delist_code) %>%  # now, the gvkey is the primary ID.
  dplyr::rename(gvkey=CCMID)

delist_data_clean$gvkey=sprintf("%06d", as.numeric(delist_data_clean$gvkey)) #converts numeric to string (character)
delist_data_clean$gvkey=factor(delist_data_clean$gvkey)

quarter=lubridate::quarter(delist_data_clean$Delist_date)
yq <- as.yearqtr(delist_data_clean$Delist_date, format = "%Y-%quarter")
delist_year_quarter= format(yq, format = "%Y-0%q")


delist_data_clean %<>%
mutate(delist_year_quarter=delist_year_quarter)


#table(delist_data_clean$Exchange) # to check!  # Header values are 1, 2, 3, 4 or 5, which correspond to the NYSE, NYSE MKT, NASDAQ, Arca and Bats respectively. Others available
# "NAICS" better than SIC

save(delist_data_clean, file= "./_DATABASE/[CLEAN] Delist_data_clean.RData")
