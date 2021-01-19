#CRSP delisting firms dataset
#Using permco ID and not GVKEY.
# we'll use the link table

rm(list=ls())


# Ctrl + shift + M = pipe operator


library(dplyr)
library(DBI)
library(tidyr)
library(tibble)


conn <- nuvolos::get_connection()
delist_data <- dbGetQuery(conn,"SELECT PERMNO,PERMCO,HEXCD,HSICCD,DLSTDT,DLSTCD
                             FROM \"MSEDELIST\"
                             WHERE (DLSTCD like '4_%_%' OR DLSTCD = '572' OR DLSTCD = '574')")
# any delist codes which starts with 4 (liquidation code), 572 or 574 (bankrupcy code)

link_data <- dbGetQuery(conn,"SELECT CCMID, LPERMCO
                             FROM \"LINK_HISTORY\"")


link_data %<>% 
            filter(LPERMCO>0) %>%
            group_by(CCMID) %>%
            unique()
View(link_data)

NB_link_data = link_data %>% count(CCMID)
NB_delist_data = delist_data %>% count(PERMCO)

# gvkey= CCMID Compustat’s permanent identifier
# LPERMCO = CRSP PERMCO link

#  Add CCMID to delist data !
delist_data = inner_join(delist_data,link_data, by=c("PERMCO"="LPERMCO"))

# now

# delist_data_compiled  =  data of delisted companies (bankrupted)

Compiled_data$gvkey=factor(Compiled_data$gvkey)
delist_data$CCMID= factor(delist_data$CCMID)





Compiled_data %>%
              group_by(gvkey) %>%
              top_n(gvkey, 1)

full_data= left_join(Compiled_data,delist_data, by=c("gvkey"="CCMID"))


 #Summary

#fin ratios of 
  
firmfin















# pre processing a faire :
as.Date(delist_data_compiled$DLSTDT)

summary(delist_data_compiled$DLSTDT)


#now. merge monthly financials avec delist_data  ( il y aura des blancs  : ceux qui ont pas default !)


# nb de defaults par années / stock markets/ etc..






# PERMNO is a unique five-digit permanent identifier
#assigned by CRSP to each security in the file. 
#Unlike CUSIP, TICKER, and COMNAM, the PERMNO neither changes
#during an issue's trading history, nor is reassigned after
#an issue ceases trading

# DLSTDT 	Delisting Date

# DLSTCD 		Delisting Code
# http://www.crsp.org/products/documentation/delisting-codes


# Liquidations
# Code	Description
# 400	Issue stopped trading as result of company liquidation.
# 401	Issue liquidated, for issue trading on NYSE.
# 403	Issue liquidated for issue trading on NASDAQ.
# 450	Issue liquidated, final distribution verified, issue closed to further research.
# 460	Issue liquidated, no final distribution is verified, issue closed to further research.
# 470	Issue liquidated, no final distribution is verified, issue pending further research.
# 480	Issue liquidated, no distribution information is available, issue is pending further research.
# 490	Issue liquidated, no distributions are to be paid, issue closed to further research.


# 572*	Delisted by current exchange - company request, liquidation.
# 574	Delisted by current exchange - bankruptcy, declared insolvent









#PERMCO is a unique permanent identifier assigned by CRSP
#to all companies with issues on a CRSP file

#HEXCD displays the latest exchange code listed for a specific security.
#Valid hexcd's are 1, 2, or 3, 
#which correspond to the NYSE, AMEX, and Nasdaq respectively.

# HSICCD 	Standard Industrial Classification Code (SIC)

