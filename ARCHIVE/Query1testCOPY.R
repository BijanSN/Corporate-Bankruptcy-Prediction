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


### we want : crsp


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
View(data)

############  Query 2 :



res <- dbSendQuery(wrds, "SELECT * FROM comphead WHERE gvkey IN firmsID")
data <- dbFetch(res, n=-1) # take all from selected
dbClearResult(res)
data

clean_data= na.omit(data)
#View(clean_data)



firmsID=readLines("Compagny_gvkey.txt")
firmsID= paste(firmsID,collapse="\n")





res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='crsp'
                   and table_name='ccmxpf_linktable'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data









############  Query 2 :

res <- dbSendQuery(wrds, "select * from ccmxpf_linktable")
data <- dbFetch(res, n=-1) # take all from selected
dbClearResult(res)
data

clean_data= na.omit(data)
#View(clean_data)













# we want : conml (name of the compagny!),gvkey (the link) , gsector,sic 


############
ccmlinktable 

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
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

View(data)

save(data, file = "Compagny_Default.RData")




clean_data=na.omit(data)

uniquekey=distinct(clean_data,gvkey)
N=nrow(uniquekey) #351 firms




ID=which(clean_data$gvkey ==uniquekey[1,1])
 #last element of the firm

clean_data[ID]


clean_data$gvkey[1]
View(uniquekey)

for(i in N){
  ID=which(clean_data$gvkey ==uniquekey[i,1])
  #ID[length(ID)]
}

  
 # if(clean_data$gvkey[i] == uniquekey)
 #   print(clean_data$splticrm)
#}




#if(dataNames$gvkey == data$

#data[]
View(data$splticrm)

if(data$splticrm=='D'){
  print(data$gvkey)
}


res2 <- dbSendQuery(wrds, "select a.conm, a.gvkey, 
                   from COMPHEAD a join adsprate b
                   on a.gvkey = b.gvkey")
data <- dbFetch(res2, n = -1)
dbClearResult(res)
data
View(data)
   
   







#-------------------------fundamentals : 




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


### we want : wrdsapps


######### see the datasets :

res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='wrdsapps'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
View(data)

######## we want : comphead  (link gvkey and name)





res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='wrdsapps'
                   and table_name='firm_ratio'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data
View(data)











save.image()
