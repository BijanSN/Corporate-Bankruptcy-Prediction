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
library(IDPmisc)
#-------------------------------------------------------------
#-------- Loading Annual Financial dataset  ----

load("./_DATABASE/[RAW] Annual_Data.RData")

#--------- Preprocessing  -------

annual_data %<>%
  group_by(GVKEY,DATADATE) %>% 
  #filter(!str_detect(SICH, "^6"))            %>%                   # non financial firms only. SICH= Standard Industrial Classification (SIC)
  filter_at(vars(ACT,AO,CH), any_vars(!is.na(.)))                  # remove if these variables are not (co-)present.

# vis_miss(annual_data[1:10000,])
# vis_miss(annual_data[10000:29000,])

# no patterns on missing values given time nor givenkeys. XRD 50% NA: removed

annual_data %<>% select(-XRD)

# Currency correction :
annual_data %<>% select(-SICH)
annual_data %<>% select(-CURNCD) # Currency code

annual_numeric_only= subset(annual_data, select=c(-GVKEY,-DATADATE,-FYEAR))*annual_data$CURRTR  #remove temporary the non numeric column for currency correction
#annual_numeric_data[,-c(1:3)]= annual_data[,-c(1:3,)]*annual_data$CURRTR 
annual_numeric_only= annual_numeric_only*annual_numeric_only$CURRTR

# Add it again
annual_data= annual_numeric_only %<>%  mutate(GVKEY= annual_data$GVKEY) %>% 
                          mutate(DATADATE= annual_data$DATADATE) %>% 
                          mutate(FYEAR= annual_data$FYEAR)

annual_data %<>% select(-CURRTR)


annual_data %<>% na.omit()

#remove "outliers" (to improve/check if not too many defaults from outliers)
annual_data %<>% filter(AT<quantile(AT,0.90,na.rm = T) & AT>quantile(AT,0.1, na.rm = T)) %>% 
                 filter(CH<quantile(CH,0.90, na.rm = T) & CH>quantile(CH,0.1, na.rm = T))


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


#vis_miss(annual_data[1:10000,]) no missing values, except for COC COGS_SAL INVT_SAL ... 

annual_data %>% select(-COC,-COGS_INV,-COGS_SAL,-INV_SA)

annual_data_clean= IDPmisc::NaRV.omit(annual_data) # omit INF values

annual_data_clean %<>% mutate(DATADATE=format(DATADATE, "%Y"))


financial_plot= annual_data_clean %>% ggplot(., aes(x=DATADATE)) + # ,fill=Delist_code
                                      geom_bar(stat="count")+
                                      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                                      labs(title="Financial data availability per year",
                                           x ="Years", y = "Counts")


save(financial_plot, file= "./_FIGURES/Financial_Data.png")

save(annual_data_clean, file= "./_DATABASE/[CLEAN] Annual_Data.RData") # no standardization yet. (end of file)

#------------------------ Footnotes_Annual_Data  -------

# load("./_DATABASE/[RAW] Footnotes_Annual_Data.RData")
# 
# Footnotes_Annual_Data %<>%  filter(AT_FN == 'TL')
# 
# #------------- Comp data




#---------- Ratings dataset 2  (nuvolous)  --------

load("./_DATABASE/[RAW] Ratings_data.RData")

# ratings_data %<>%  na.omit() %>% 
#                    mutate(DATADATE=format(DATADATE, "%Y"))

ratings_dataset= ratings_data %>%                                                                                   # pipe opperator "%<>%" applies the right-hand-side functions and assigns to L.H.S.
                                  na.omit() %>%                                                                     # x %>% f(y) =  f(x,y). removes uncomplete data
                                  dplyr::rename(ratings=SPLTICRM) %>%                                               # rename to something more clear
                                  filter(!ratings %in% c("N.M.","SD","Suspended")) %>%                              # remove unusable ratings
                                  mutate(GVKEY=factor(GVKEY)) %>%
                                  dplyr::group_by(GVKEY) %>%
                                  mutate(DATADATE=format(DATADATE, "%Y"),                                                # convert from 'numeric' to 'Date' type
                                         ratings= factor(ratings,levels=c("AAA","AA+","AA", "AA-",
                                                                          "A+","A","A-","BBB+","BBB",
                                                                          "BBB-", "BB+","BB","BB-",
                                                                          "B+","B-","B","CCC+","CCC",
                                                                          "CCC-","CC","C","D"))) %>% #,       # convert from 'character' to 'factor' type, in quality's descending order
                                         
                                         #defaultbool= ifelse(ratings=='D',1,0),                                     # create a new column "default_bool" =1 if the compagny defaults at time t=i
                                         #defaultfuture = max(defaultbool),                                          # create bool if the company will default in the future.
                                         #oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0)) %>% #,                       # create bool =1 if defaults next year ( add "& ratings !="D" ")
                                         
                                         
                                         # defaultdate=dplyr::case_when(defaultfuture==1 ~min(DATADATE),              # create a new column with the default date, if applicable
                                         #                              defaultfuture==0 ~as.Date('2100-12-12'))) %>% # can't put 'NA' directly : I use a fake date to convert afterwards.
                                         # 
                                  
                                  droplevels()    



ratings_plot= ratings_dataset %>% ggplot(., aes(x=DATADATE,fill=ratings)) + # ,fill=Delist_code
                      geom_bar(stat="count")+
                      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                      labs(title="Ratings data per year",
                           x ="Years", y = "Counts")

# 1985-2017
#last_ratings_dataset= ratings_dataset %>% group_by(GVKEY) %>% 
                                          #dplyr::filter(DATADATE==max(DATADATE)) # only one obs per year

#last ratings of a compagny ( per year) :
#ratings_plot2= last_ratings_dataset %>% ggplot(., aes(x=DATADATE,fill=ratings)) + # ,fill=Delist_code
  # geom_bar(stat="count")+
  # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  # labs(title="Ratings data per year",
  #      x ="Years", y = "Counts")


#one per year for each company :

ratings_dataset_one_per_year=  ratings_dataset %>%
                                                group_by(GVKEY,DATADATE) %>%
                                                distinct(DATADATE, .keep_all = TRUE) %>% 
                                                arrange(GVKEY,DATADATE)


#final year ratings of each companies 
# ratings_dataset__last_only=  last_ratings_dataset %>%
#                                                   group_by(GVKEY) %>% 
#                                                   summarise_all(last)


# ratings_plot_final= ratings_dataset__last_only %>% ggplot(., aes(x=DATADATE,fill=ratings)) + # ,fill=Delist_code
#   geom_bar(stat="count")+
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
#   labs(title="Final ratings data",
#        x ="Years", y = "Counts")

# ADD BOOLS !

RATINGS=ratings_dataset_one_per_year  %>% mutate(defaultbool= ifelse(ratings=='D',1,0),                                     # create a new column "default_bool" =1 if the compagny defaults at time t=i
                                          defaultfuture = max(defaultbool),                                          # create bool if the company will default in the future.
                                          oneyeardefaultpred = ifelse(lead(ratings)=='D',1,0))#,                       # create bool =1 if defaults next year ( add "& ratings !="D" ")
                                            
                                            
                                            # defaultdate=dplyr::case_when(defaultfuture==1 ~min(DATADATE),              # create a new column with the default date, if applicable
                                            #                              defaultfuture==0 ~as.Date('2100-12-12')))    # can't put 'NA' directly : I use a fake date to convert afterwards.


RATINGS_D= RATINGS %>%  filter(ratings=='D')
RATINGS_D$DATADATE= as.Date(as.numeric(RATINGS_D$DATADATE)) # for merging



ratings_plot_D= RATINGS_D %>% ggplot(., aes(x=DATADATE)) + # ,fill=Delist_code
  geom_bar(stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(title="Ratings D ",
       x ="Years", y = "Counts")

#727 'D' to classify

# RATINGS_D_unique= RATINGS_D %>% group_by(GVKEY) %>% 
#                         count()
# 414 firms


save(ratings_plot, file= "./_FIGURES/Ratings_plot.jpeg")


save(RATINGS, file= "./_DATABASE/[CLEAN] RATINGS.RData")
save(RATINGS_D, file= "./_DATABASE/[CLEAN] RATINGS_D.RData")

save(ratings_dataset, file= "./_DATABASE/[CLEAN] Ratings_Data.RData")

#--------Delisting dataset : Preprocessing : ---------

rm(list=ls())

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


delist_data_clean$Delist_date <-format(delist_data_clean$Delist_date, "%Y")

#delist_data_clean$Delist_date <- ceiling_date(delist_data_clean$Delist_date, "month") - days(1)
#delist_data_clean$Delist_date <-format(delist_data_clean$Delist_date, "%Y-0%q")


# gvkey= CCMID Compustat’s permanent identifier
# LPERMCO = CRSP PERMCO link

# replace CRSP's PERMCO with COMPUSTAT's CCMID/gvkey :
delist_data_clean = inner_join(delist_data_clean,link_data, by=c("PERMCO"="LPERMCO"))

delist_data_clean %<>% select(CCMID,Exchange:Delist_code) %>%  # now, the gvkey is the primary ID.
  dplyr::rename(gvkey=CCMID)

delist_data_clean$gvkey=sprintf("%06d", as.numeric(delist_data_clean$gvkey)) #converts numeric to string (character)
delist_data_clean$gvkey=factor(delist_data_clean$gvkey)

# quarter=lubridate::quarter(delist_data_clean$Delist_date)
# yq <- as.yearqtr(delist_data_clean$Delist_date, format = "%Y-%quarter")
# delist_year_quarter= format(yq, format = "%Y-0%q")

#table(delist_data_clean$Exchange) # to check!  # Header values are 1, 2, 3, 4 or 5, which correspond to the NYSE, NYSE MKT, NASDAQ, Arca and Bats respectively. Others available
# "NAICS" better than SIC

delist_data_clean$Delist_date=factor(delist_data_clean$Delist_date) 


barplot(prop.table(table(delist_data_clean$Delist_date))) #shortens the study timeframe post 2001 era ?
# fancier :

#80k firms delisting :

delist_plot= ggplot(delist_data_clean, aes(x=Delist_date)) + # ,fill=Delist_code
  geom_bar(stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(title="Delisting date for bankrupcy reasons",
       x ="Years", y = "Counts")


save(delist_plot, file="./_FIGURES/Delist_data.jpeg")

save(delist_data_clean, file= "./_DATABASE/[CLEAN] Delist_data_clean.RData")

# -------------- Cleaning & Visualisation -----------------
#load("./_DATABASE/[RAW] Annual_Data2.RData")

# glimpse(annual_data)
# vis_miss(annual_data[1:10000,])

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


#summary(annual_data)

#  FYEAR
#Min.   :1984
#Max.   :2019


# Standardization
#summary(annual_data)

# preproc1 <- preProcess(test, method=c("center", "scale")) 
# test2 <- predict(preproc1,test)
# summary(test2)
# 
 normalize <- function(x, na.rm = TRUE) (x - mean(x, na.rm = T))/sd(x, na.rm = T) # Changes scale. Mean = 0 sd = 1.
# 
# 
# # Standardization
#norm_data= annual_data %>% 
#                           mutate_at(vars(AT:SAL_TA), normalize)  # values = number of std from the mean

#boxplot(annual_data$EBIT)


#save(norm_data, file= "./_DATABASE/[RAW] Annual_Data_norm.RData")
# ---------------------------
# 
# t1= annual_data %>%  group_by(GVKEY) %>% 
#                  select(GVKEY,EBIT) %>% 
#                  summarise(min= min(EBIT)) %>% 
#                  summarise(max= max(EBIT)) 
#   
#   
# boxplot(annual_data$COGS)
# 
# annual_data %>%
#   ggplot(., aes(x="",y=EBIT)) + 
#   geom_boxplot(outlier.colour=NA)
# 
# 
# annual_data %>%
#                filter(EBIT<10000) %>% 
#                ggplot(., aes(EBIT)) +
#                scale_x_continouous(limits=c(0, 1000)) +
#                geom_histogram()
#               
# 
# annual_data$EBIT
# 
# ggplot(annual_data, aes(EBIT)) + 
# geom_histogram()
# 
# 
# ggplot(annual_data, aes(x = EBIT)) +
#   geom_bar()
# 
# 
# filter(annual_data$EBIT<25000)

 
# annual_data %>%
#                group_by(GVKEY) %>%
