library(RPostgres)
#install.packages("dplyr")
library(dplyr)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='bijansn',
                  password='PasswordForWRDS123')


######## see all the available libraries : 

res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data
View(data)


### imagine we want : crsp


######### see the datasets :

res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='crsp'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
View(data)

######## we want : comphead  (link gvkey and name)


res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='crsp'
                   and table_name='comphead'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data


# we want : conml (name of the compagny!),gvkey (the link) , gsector,sic 


############


############  Query 1 :


res <- dbSendQuery(wrds, "select conml,gvkey,gsector,sic
                   from comphead")
data <- dbFetch(res, n=-1) # take all companies
dbClearResult(res)
data

clean_data= na.omit(data)
#View(clean_data)


#Query 2 : 


res <- dbSendQuery(wrds, "select gvkey, splticrm, datadate from adsprate")
data <- dbFetch(res, n=10000)
dbClearResult(res)
data
#View(data)

clean_data=na.omit(data)

distinct(clean_data,gvkey)




#Query 1 :

res <- dbSendQuery(wrds, "select * from crsp.dsf")
data <- dbFetch(res, n=10)
dbClearResult(res)
data
View(data)




res <- dbSendQuery(wrds, "select * from adsprate")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

View(data)





#if(dataNames$gvkey == data$











res2 <- dbSendQuery(wrds, "select a.conm, a.gvkey, 
                   from COMPHEAD a join adsprate b
                   on a.gvkey = b.gvkey")
data <- dbFetch(res2, n = -1)
dbClearResult(res)
data
View(data)
   
   

   
   
   
   
   
   
   


save.image()
