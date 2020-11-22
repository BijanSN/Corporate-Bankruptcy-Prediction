##################### 
#Step 1 : Query the data from the WRDS Database.
##################### Clear environment:
#rm(list=ls())
##################### Setup :
library(RPostgres) #doesn't work on the cloud
library(dplyr)

#setwd("C:/Users/Vida/Desktop/Th?se/Corporate Default Prediciton/")
####################

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='bijansn',
                  password='*********')



# Query Ratings : -------------------

res <- dbSendQuery(wrds, "select gvkey, splticrm, datadate from adsprate")
data <- dbFetch(res, n=-1)
dbClearResult(res)

save(data, file = "./_DATABASE/[RAW] Compagny_Default.RData")


# Query Compagny financials : -------------------

# res <- dbSendQuery(wrds, "select column_name
#                    from information_schema.columns
#                    where table_schema='wrdsapps'
#                    and table_name='firm_ratio'
#                    order by column_name")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# 
# 
# res <- dbSendQuery(wrds, "select * from firm_ratio")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)


