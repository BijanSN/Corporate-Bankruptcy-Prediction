# New import
library(DBI)

conn <- nuvolos::get_connection()
financial_data <- dbGetQuery(conn,"SELECT CCMID,DATADATE
                                   FROM \"BALANCE_SHEET_QUARTERLY\"
                                   limit 10000")

# hist_data <- dbGetQuery(conn,"SELECT * FROM \"COMPANY_HISTORY\" limit 10000")
# ratings_data <- dbGetQuery(conn,"SELECT * FROM \"COMPANY_RATINGS\" limit 10000")
income_statement_data <- dbGetQuery(conn,"SELECT * FROM \"INCOME_STATEMENT_QTRLY\"")





# desc_data <- dbGetQuery(conn,"SELECT * FROM \"COMPANY_DESCRIPTION\"")
# desc_data$CCMID=sprintf("%06d", as.numeric(desc_data$CCMID))
# 
# CIK<-desc_data[,1:2]





