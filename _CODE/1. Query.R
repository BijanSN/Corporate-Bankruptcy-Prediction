##################### 
#Step 1 : Query the data from the WRDS Database.
##################### Clear environment:
#rm(list=ls())
##################### Setup :
library(RPostgres)
library(dplyr)
setwd("C:/Users/Vida/Desktop/Th?se/Corporate Default Prediciton/")
####################

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='bijansn',
                  password='PasswordForWRDS123')


res <- dbSendQuery(wrds, "select gvkey, splticrm, datadate from adsprate")
data <- dbFetch(res, n=-1)
dbClearResult(res)

save(data, file = "./_DATABASE/[RAW] Compagny_Default.RData")
