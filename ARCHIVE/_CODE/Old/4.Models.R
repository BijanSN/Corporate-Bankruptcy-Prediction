######################################################
# Step 4 : Appying models
######################################################

rm(list=ls())

#------------------- Setup --------------------------------------------------

library(dplyr)
library(magrittr)
library(ggplot2)
library(lattice)
library(caret)
library(AppliedPredictiveModeling)
library(mlbench)

set.seed(100)
#-------------------Loading the data ----------------------------------------

load("./_DATABASE/[Clean] Compiled_Data.RData")
load("./_DATABASE/[Clean] Delist_Data.RData")

#-------------------Logit models --------------------------------------------


str(Compiled_data2$gvkey)
str(delist_data_clean$gvkey) 


Compiled_data2$gvkey=factor(Compiled_data2$gvkey)
delist_data_clean$gvkey= factor(delist_data_clean$gvkey)


final_data= left_join(Compiled_data2, delist_data_clean ,by=c("gvkey"="gvkey"))

# final_data %>% 
#             select(gvkey,ratings:defaultdate,Exchange:Delist_code) %>% 
#             View()



# graph dates of delisting. Drops days (to implement before)
# final_data$Delist_date <- format(final_data$Delist_date, "%Y-%m")
# final_data$defaultdate <- format(final_data$defaultdate, "%Y-%m")
# final_data$datadate <- format(final_data$datadate, "%Y-%m")

table(factor(final_data$Delist_date))
barplot(table(factor(final_data$Delist_date)))


# View(final_data$Delist_code)
table(final_data$Delist_code, useNA = "ifany") # only 574's (the bankrupcy code) in the dataset.

# (optional) find a new finratio datasets with its ccmid, not only 574's (selected through web query)





# yearly delistings : à  mettre en unique !

final_data_once %>% arrange(desc(datadate)) %>%
               group_by(gvkey)         %>%
               filter(datadate== max(datadate))


final_data$Delist_date <- format(final_data$Delist_date, "%Y")
final_data$defaultdate <- format(final_data$defaultdate, "%Y")
final_data$datadate <- format(final_data$datadate, "%Y")

table(factor(final_data$Delist_date))
barplot(table(factor(final_data$Delist_date)))

# années avec trous.....







#Simple model :
#ROE(profitability), quick ratio(liquidity),book to market (valutation) on default predictors


# Compiled_data2 %>%
#   lm(defaultbool ~ roe + bm + quick_ratio)


model1=lm(Compiled_data2$oneyeardefaultpred ~ Compiled_data2$roe + Compiled_data2$bm + Compiled_data2$quick_ratio + Compiled_data2$datadate )
model2=lm(Compiled_data2$oneyeardefaultpred ~ ., data=Compiled_data2)


summary.lm(model2)
# Adjusted R-squared:  0.9151 
# F-statistic: 24.68 on 908 and 1086 DF,  p-value: < 2.2e-16
#using even ratings ... (shouldn't)


model3=lm(Compiled_data2$defaultbool ~ ., data=Compiled_data2)
summary.lm(model3)
#perfect fit obviously, ratings d= default bool


cor(Compiled_data2)

#----------- Caret package -------------------------------------------------


transparentTheme(trans = .4)
# threeratio= Compiled_data2 %>%
#                        ungroup() %>%
#                        select(roe,bm,quick_ratio)
#                             
RegressionVariables=c("roe","bm","quick_ratio")

# featurePlot(x = Compiled_data2[,RegressionVariables], 
#             y = Compiled_data2$defaultbool, 
#             plot = "pairs")


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)


featurePlot(x = Compiled_data2[,RegressionVariables],
            y = Compiled_data2$defaultbool,  
            plot = "scatter", 
            layout = c(3, 1))
#some cleanings to do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




Compiled_data2= na.omit(Compiled_data2)


#----------------------------------------------------------------------------------------------------------

inTrain <- createDataPartition(  # stratified random split of the data
  y = Compiled_data2$defaultbool, ## the outcome data 
  p = .75,## The percentage of data in the training set
  list = FALSE
)

training <- Compiled_data2[ inTrain,]
testing  <- Compiled_data2[-inTrain,]

# nrow(training)
# nrow(testing)


table(Compiled_data2$defaultbool)
table(Compiled_data2$oneyeardefaultpred)



ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3#,
  #classProbs = TRUE, 
  #summaryFunction = twoClassSummary
)


str(Compiled_data2[,RegressionVariables])

#Compiled_data2$oneyeardefaultpred= na.omit(Compiled_data2$oneyeardefaultpred)

Compiled_data2$oneyeardefaultpred= as.factor(Compiled_data2$oneyeardefaultpred)
#x= Compiled_data2[,RegressionVariables]



true_data= preProcess(Compiled_data2[,"bm"], 
       method = "knnImpute",   # or *bagImpute* / *medianImpute*
       pcaComp = 10,
       na.remove = TRUE,
       k = 5,
       knnSummary = mean,
       outcome = NULL,
       fudge = .2,
       numUnique = 3,
       verbose = TRUE,
)


plsFit <- train(
  x= Compiled_data2[,"bm"],
  y= Compiled_data2$oneyeardefaultpred,
  data = training,
  method = "cforest",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC",
  na.action = na.omit
)

plsFit

# models :
#https://topepo.github.io/caret/train-models-by-tag.html

#Altman_Z=3.3*(EBIT/AT) +0.99*(SALE/AT) +0.6*(ME/LT) +1.2*(ACT/AT) +1.4*(RE/AT);





