# new quaterly queries

library(DBI)
library(naniar)


conn <- nuvolos::get_connection()

GICS <- dbGetQuery(conn,"SELECT gvkey, gsector
                         FROM \"CO_HGIC\"")


eco_data <- dbGetQuery(conn,"SELECT *
                             FROM \"ECIND_MTH\"
                             WHERE ECONISO = 'USA'
                             limit 10000")
# DATE ?

financial_data <- dbGetQuery(conn,"SELECT GVKEY,TIC,CIK ,DATADATE ,FYEARQ ,FQTR ,ACTQ, AOQ, ATQ,CHEQ,
                                          EPSPXQ,INVTQ ,LCTQ ,LTQ ,NIQ ,OIADPQ ,PIQ ,PPENTQ ,REVTQ ,SALEQ ,DVPSPQ
                                   FROM \"FUNDQ\"
                                   limit 50000 ")

# CAPXY ,AQCY ,COGSY ,ESUBCY ,NIY,PIY ,REVTY,SALEY
vis_miss(financial_data[20000:30000,])#, 'warn_large_data'= F)




# modd1=lm(financial_data$NIY~financial_data$PIY + financial_data$PIQ)
# summary(modd1)


# diff PIQ et PIY = quaterly et quaterly pretax income. we need only Quaterly.


#GVKEY Global Company Key
#TIC
#DATADATE Date
#FYEARQ Fiscal Year
#FQTR Fiscal Quarter

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
#DPY Depreciation and Amortization - Total
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

# ALtman Z score :
# WCAPQ Working Capital 
# ATQ Total Assets
# WCAPQ/ATQ

#REQ Retained earnings
#REQ/ATQ

#EBIT : top down
# Net Income + Interest + Taxes
# NIY + 
#XINTQ Interest and Related Expense- Total
#TXTQ Income Taxes - Total
#
#
# EBIT : bottom up
# Revenue – Cost of Goods Sold – Operating Expenses

# REVTY Revenue - Total
# COGSQ  Cost of Goods Sold
# XOPRQ Operating Expense- Total

# mouais : .---------------------------------------------------------------
index_data <- dbGetQuery(conn,"SELECT GVKEYX, DATADATE,CSTKQ  
                                   FROM \"IDX_QRT\"
                                   limit 10000")

# GVKEYX Global Index Key
# DATADATE
# CSTKQ  Common/Ordinary Stock (Capital) ----------------------------------



market_data <- dbGetQuery(conn,"SELECT *
                                   FROM \"CO_AMKT\"
                                   limit 10000")



# CCMID
# DATADATE
# MKVALTQ  Market Value - Total




# hist_data <- dbGetQuery(conn,"SELECT * FROM \"COMPANY_HISTORY\" limit 10000")
# ratings_data <- dbGetQuery(conn,"SELECT * FROM \"COMPANY_RATINGS\" limit 10000")
income_statement_data <- dbGetQuery(conn,"SELECT * FROM \"INCOME_STATEMENT_QTRLY\"")
