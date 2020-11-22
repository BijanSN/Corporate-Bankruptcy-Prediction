####################
# Step 1 : Queries #
####################

rm(list=ls())

#------------------ Setup --------------------------

library(DBI)
library(dplyr)

conn <- nuvolos::get_connection()

#------------------ Queries ------------------------

#try to change FUNDQ with balencesheet + income data to see if there's more 

#--------Financial dataset (quaterly):

financial_data <- dbGetQuery(conn,"SELECT GVKEY,DATADATE,FYEARQ,FQTR,ACTQ,AOQ,ATQ,CHEQ,COGSQ,CURRTRQ,CURNCDQ,CURUSCNQ,
                                          DVPSPQ,DPY,EPSPXQ,INVTQ ,LCTQ ,LTQ ,NIQ ,OIADPQ ,PIQ ,PPENTQ ,REQ,REVTQ,
                                          SALEQ, WCAPQ, XOPRQ, CEQQ,PRCCQ,CSHOQ,CHQ,UINVQ
                                   FROM \"FUNDQ\"")

save(financial_data, file= "./_DATABASE/[RAW] Financial_Data.RData")
rm(financial_data) # for memory purposes



#--------Financial dataset (annualy):

annual_data <- dbGetQuery(conn,"SELECT GVKEY,DATADATE,FYEAR,AT,ACT,AO,CAPX,CH,CHE,COGS,CSHO,CURRTR,CURNCD,
                                          DVPSP_C,DP,EPSPX,INVT,LCT,LT,NI,OIBDP,OIADP,PI,PPENT,RE,REVT,
                                          SALE, WCAP,XOPR,CEQ,PRCC_C,IBC,XINT,DLC,DVT,EBIT,EBITDA,BKVLPS,DLTT,XRD,SICH,SEQ,DVP,DVC,PRSTKC,IB,ICAPT
                                   FROM \"FUNDA\"")

save(annual_data, file= "./_DATABASE/[RAW] Annual_Data.RData")


#-------- Financial dataset footnotes(annual): 

Footnotes_Annual_Data <- dbGetQuery(conn,"SELECT GVKEY,DATADATE,FYEAR,AT_FN
                          FROM \"FUNDA_FNCD\"")

save(Footnotes_Annual_Data, file= "./_DATABASE/[RAW] Footnotes_Annual_Data.RData")


#-------- Comp data


Comp_Data <- dbGetQuery(conn,"SELECT GVKEY, DLRSN
                          FROM \"COMPANY\"
                          WHERE DLRSN= '02'" )

save(Comp_Data, file= "./_DATABASE/[RAW] Comp_Data.RData")

# Companies are removed due to bankruptcy when this variable is ‘02’
# liquidation when it is ‘03’

#--------Delisting dataset :


delist_data_raw <- dbGetQuery(conn,"SELECT PERMNO,PERMCO,HEXCD,HSICCD,DLSTDT,DLSTCD
                             FROM \"MSEDELIST\"
                             WHERE (DLSTCD like '4_%_%' OR DLSTCD = '572' OR DLSTCD = '574')")

# any delist codes which starts with 4 (liquidation code), 572 or 574 (bankrupcy code)

save(delist_data_raw, file= "./_DATABASE/[RAW] Delist_data_raw.RData")
rm(delist_data_raw)

#--------Link dataset :


link_data <- dbGetQuery(conn,"SELECT CCMID, LPERMCO
                             FROM \"LINK_HISTORY\"")

link_data %<>%
  filter(LPERMCO>0) %>%
  group_by(CCMID) %>%
  unique()

save(link_data, file= "./_DATABASE/[RAW] Link_data.RData")
rm(link_data)


#---------- Ratings Dataset

ratings_data <- dbGetQuery(conn,"SELECT GVKEY, DATADATE, SPLTICRM
                             FROM \"ADSPRATE\"")

save(ratings_data, file= "./_DATABASE/[RAW] Ratings_data.RData")
rm(ratings_data)
