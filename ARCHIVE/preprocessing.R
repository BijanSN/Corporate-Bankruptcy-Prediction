#Load Database
load("Compagny_Default.RData")

Ratings_data= data.frame(data, stringsAsFactors = T)
head(Ratings_data)
str(Ratings_data)

Ratings_data=Ratings_data[,c('gvkey','datadate','splticrm')]
head(Ratings_data)
str(Ratings_data)

#Clean it 
clean_data=na.omit(Ratings_data)

#stats
key=clean_data$gvkey
uniquekey=distinct(clean_data,gvkey)

# number of unique firms monthly ratings
t=data.frame(clean_data %>% count(clean_data$gvkey)) # to add latest ratings !
nrow(t)
t$clean_data.gvkey
hist(t$n, main = 'Histogram of the dataset\'s monthly ratings', xlab='Months')


t_ratings= table(clean_data$splticrm)

as.data.frame(rbind(t_ratings))

t_ratings_df=t(data.frame(t_ratings))

#tibble(t_ratings_df) %>% relocate("A")

barplot(t_ratings)











# for each unique clean key (t$clean.data.gvkey), i want the latest(= using the number of occ (t$n)) rating ( clean data$spltcrm) :

for (key in t$`clean_data$gvkey`){
  print()
}








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