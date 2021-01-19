#---- new modelling --#

rm(list=ls())

load("./_DATABASE/[CLEAN] MODEL_FINAL_data.RData")

source("./_CODE/99 -Functions.R")
scale_me <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}

set.seed(10)

library(glmnet)
#----------------------------------------------------------------#

TOTAL_DELISTED %<>% 
  mutate(                                      # Liquidity / Capital Structure
    MKVALT=CSHO*PRCC_C, # Market value
    MKTVAL_LT=(PRCC_C*CSHO)/LT,
    
    WC_TA= WCAP/AT,
    
    CSH_FLOW=((IBC+DP)/AT), #Cash Flow
    CSH_HOLD=(CHE/AT), 
    CSH_FLOW_CL= CSH_FLOW/LCT, # Cash Flow/Current Liabilities 
    CSH_FLOW_LT= CSH_FLOW/LT, # Cash Flow/Total Debt   
    
    CA_CL=(ACT/LCT),   # Current ratio : Current Assets/Current Liabilities
    CHE_CL= (CHE/LCT), # Cash+E/Current Liabilities   
    CHE_AT= (CHE/AT), # Cash+E/Total Assets
    CL_E=(LCT/MKVALT),  # Current Liabilities/Equity 
    CA_TD= (ACT/LT),   # Current Assets/Total Debt 
    
    
    MKT_TA=(MKVALT/AT), 
    #ST_TD=LCT/(DLTT+DLC), # Short term debt /Book value of Debt 
    #LVRG=(DLTT+DLC)/SEQ, #Leverage
    
    # # Size ("flat" numbers)
    # LOG_TA=log(AT),
    # LOG_SAL=log(SALE),
    # LOG_CAPX=log(CAPX),
    # # new :
    # LOG_XOPR=log(XOPR),
    # LOG_CEQ=log(CEQ),
    # LOG_XINT=log(XINT),
    
    # Growth:
    CSH_CAPX= CSH_FLOW/CAPX,
    CH_OP_MAR= OIBDP/SALE, # change in operating margins
    
    #Profitability
    
    EPS=NI/CSHO,
    ROA=NI/AT,
    ROE=NI/(CSHO*PRCC_C),
    #LOG_SAL_TA=log(SALE/AT),
    EBIT_TA= EBIT/AT,
    EBIT_SAL= EBIT/SALE,
    # EBIT_INVCAP=EBIT/ICAPT, # EBIT/Invested Capital ! divided by 0
    NI_TA= NI/AT,           # NI/Total Assets 
    SAL_TA=SALE/AT,
    #Others: 
    #COC=XINT/DLC,
    #LOG_COC=log(XINT/DLC),  #Cost of Capital (Interest expenses/total debt), after tax
    RE_TA=RE/AT,            #REQ Retained earnings/total assets
  )

TOTAL_DELISTED %<>%  na.omit()

#Illusatrations :
# 
# TAB= TOTAL_DELISTED %>% dplyr::select(gvkey,FYEAR,ACT,LCT,SALE,EBIT)
# TAB=TAB[c(22,45,836,33,63,440,775,499,992,1200,1400,520,600,2000,3000),]
# stargazer(data.frame(TAB),summary = F)




# for Altmans's Z score :

X1=1.2
X2=1.4
X3=3.3
X4=0.6
X5=1

TOTAL_DELISTED %<>%  mutate(Zscore= X1*WC_TA+X2*RE_TA+X3*EBIT_TA+X4*MKTVAL_LT+X5*SAL_TA,
                            Z_bool= ifelse(Zscore<1.8,1,0))
# SOME GRAPHS :
ggplot(ONLY_DELISTED_T_1, aes(x=Exchange)) +
  geom_histogram(stat="count")

ggplot(ONLY_DELISTED_T_1, aes(x=DATADATE)) +
  geom_histogram(stat="count")+
  ggtitle("Number of delisting due to bankruptcy or liquidation, per year")+
  xlab("Years")+
  ylab("Number of observations") 
# ---------------------------------------------------------------------------------



# ALL_FIRMS_RATINGS_FINANCIALS$DATADATE=as.Date(as.numeric(ALL_FIRMS_RATINGS_FINANCIALS$DATADATE))

DATA_10Y=TOTAL_DELISTED %>% 
  filter(DATADATE =='1987'|DATADATE =='1997'|DATADATE =='2007'|DATADATE =='2017')

DATA_10Y %<>% mutate(Zscore= X1*WCAP/AT+X2*RE/AT+X3*EBIT/AT+X4*(PRCC_C*CSHO)/LT+X5*SALE/AT,
                     Z_bool= ifelse(Zscore<1.8,1,0))

# WCAP/AT : 
p1=ggplot(DATA_10Y,aes(x=WCAP/AT, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-0.4,1)
# EBIT/AT : 
p2=ggplot(DATA_10Y,aes(x=RE/AT, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-1,1)
# RE/AT : 
p3=ggplot(DATA_10Y,aes(x=EBIT/AT, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-0.4,0.4)
#(PRCC_C*CSHO)/LT
p4=ggplot(DATA_10Y,aes(x=(PRCC_C*CSHO)/LT, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-0.2,8)+ xlab("MKTVAL/LT")
#SALE/AT
p5=ggplot(DATA_10Y,aes(x=SALE/AT, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-0.2,4)

#Z-score  

p6=ggplot(DATA_10Y,aes(x=Zscore, color=DATADATE, fill=DATADATE)) + 
  geom_density(aes(y=..density..),alpha=0.25) + # alpha = intensity of colors
  xlim(-2,7)+
  geom_vline(aes(xintercept =1.8), 
             linetype = "dashed", size = 0.6)+  # below : likely to default
  geom_vline(aes(xintercept =3),               # between : unsure
             linetype = "dashed", size = 0.6) # above: safe 


p6

grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3,top="Density of Z-score's components through decades")





All_years = TOTAL_DELISTED %>% group_by(DATADATE) %>% 
  summarise(m_WCAP_TA=median(WC_TA),
            m_SAL_TA=median(SAL_TA),
            m_EBIT_TA=median(EBIT_TA),
            m_MKTVAL_LT=median(MKTVAL_LT),
            m_RE_TA=median(RE_TA),
            m_Z=median(Zscore))

#filter(!DATADATE =='1981'&!DATADATE =='1982'&!DATADATE =='1983'&!DATADATE =='1984')

All_years$DATADATE= format(as.Date(All_years$DATADATE, format="%Y"),format = "%Y")
Above_1985= All_years %>% filter(DATADATE>1985 & DATADATE<2020 )


g1=ggplot(Above_1985 ,aes(x=DATADATE,y=m_WCAP_TA,group = 1)) +
  geom_smooth(method = lm)+ 
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of working capital ratio")

g2=ggplot(Above_1985 ,aes(x=DATADATE,y=m_SAL_TA,group = 1)) +
  geom_smooth(method = lm)+ 
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of sales to total asset ratio")

g3=ggplot(Above_1985 ,aes(x=DATADATE,y= m_EBIT_TA,group = 1)) +
  geom_smooth(method = lm)+  
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of EBIT to total asset ratio")

g4=ggplot(Above_1985 ,aes(x=DATADATE,y= m_MKTVAL_LT,group = 1)) +
  geom_smooth(method = lm)+ 
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of Market Value of equity to total liabilities")

g5=ggplot(Above_1985 ,aes(x=DATADATE,y= m_RE_TA,group = 1)) +
  geom_smooth(method = lm)+ 
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of Retained earnings to total asset ratio")

g6=ggplot(Above_1985 ,aes(x=DATADATE,y=m_Z,group = 1)) +
  geom_smooth(method = lm)+ 
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("")+ ylab("Median of Zscore")


grid.arrange(g1,g2,g3,g4,g5,g6, ncol = 3 ,top = "Median of Zscore componants from 1985 to 2019")




#-------------------  SETUP : ------------------------------------
# unscaled data : 
TOTAL_DELISTED


#SCALED DATA : 

TOTAL_DELISTED_SCALED =TOTAL_DELISTED %>% as_tibble() %>% 
  dplyr::mutate(across(AT:CSHO,robust_scalar))

TOTAL_DELISTED_SCALED %<>% dplyr::mutate(across(MKVALT:Zscore,robust_scalar))

# NORM + SCALED : 

mean_norm_minmax <- function(x){
  (x- mean(x)) /(max(x)-min(x))
}


TOTAL_DELISTED_NORM =TOTAL_DELISTED %>% as_tibble() %>% 
                                               dplyr::mutate(across(AT:CSHO,mean_norm_minmax))
  
TOTAL_DELISTED_NORM %<>% dplyr::mutate(across(MKVALT:Zscore,mean_norm_minmax))


########## changed for SCALED + NORM :

split_stratified  <- initial_split(TOTAL_DELISTED_SCALED, prop = 0.8, strata = "pred_default1")
TRAIN  <- training(split_stratified) #3770 OBS
TEST   <- testing(split_stratified) # 942 OBS

table(TOTAL_DELISTED$pred_default1) %>% prop.table()
table(TRAIN$pred_default1) %>% prop.table()
table(TEST$pred_default1) %>% prop.table()

######################################################################################################
                                    
                                    #############
                                    #  models : #
                                    #############


######################            Altman' Z score                #####################################

DATA_ref= factor(TEST$pred_default1) # reference
DATA_pred= factor(TEST$Z_bool)       # predicted 


Z_cm= caret::confusionMatrix(data =DATA_pred,DATA_ref, positive= "1")
Z_cm
Z_cm= as.data.frame.matrix(Z_cm$table)

stargazer(Z_cm, summary = FALSE , title = "Confusion Matrix of the Altman's")


roc(TEST$pred_default1,TEST$Z_bool,plot = T,col="black",print.auc=T)
auc(TEST$pred_default1,TEST$Z_bool)


######################            LINEAR REGRESSIONS               ##################################

#The first one, V1, contains the entire set of parameters and their log counterpart (if applicable)
TRAIN %<>% 
  ungroup %>% 
  dplyr::select(!c(gvkey,FYEAR,FYEAR_1,FYEAR_2,FYEAR_3,FYEAR_4, 
                   DATADATE,SIC,Exchange, Delist_code,Delist_date,Zscore,Z_bool))
TEST %<>% 
  ungroup %>% 
  dplyr::select(!c(gvkey,FYEAR,FYEAR_1,FYEAR_2,FYEAR_3,FYEAR_4, 
                   DATADATE,SIC,Exchange, Delist_code,Delist_date,Zscore,Z_bool))


# Concatenate column's names into a single list
All_coeffs_names=paste(colnames(TRAIN),collapse = ", ") 

#Only advanced Ratios :
# ratios_names= c("CSH_FLOW","CSH_HOLD","COC", "EPS", "LVRG" ,"MKVALT",
#                 "MTB","ROA","ROE","WC_TA","CA_CL","CH_CL","CH_AT","CSH_FLOW_CL","CSH_FLOW_LT",
#                 "CL_E","CA_TD","QA_CL","QA_INV","WC_SAL","INV_SA", 
#                 "COGS_INV","COGS_SAL", "NI_TA" ,"NI_E","NI_SAL",         
#                 "EBIT_INVCAP","EBIT_TA","EBIT_SAL") #  "PAYOUT_R",

#Independant Variable:

y="pred_default1"

V1= glm_var(y,colnames(TRAIN))
V1

# V1_names= pred_default1 ~ AT + ACT + AO + CAPX + CHE + COGS + DP + EPSPX +
#   INVT + LCT + LT + NI + PPENT + RE + REVT +
#   WCAP + XOPR + CEQ + PRCC_C + XINT + DLC + DVT + EBIT +
#   EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT+
#   CSHO + MKVALT + MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
#   CL_E + CA_TD + log(ACT) +log(CAPX) + log(CHE)+ log(SALE)+
#   CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA +
#   EBIT_SAL  + SAL_TA + RE_TA

#### + NI_TA OIBDP MTB  LVRG not included

#remove high VIF :  IBC + SALE + CSH_FLOW_CL+  CSH_FLOW RE_TA WC_TA + SAL_TA  CL_E +CHE_CL + CHE_AT + 
# log(ACT) +log(CAPX) + log(CHE)+ log(SALE)+ 
#new V1 : ratios only 
V1_names= pred_default1 ~ MKTVAL_LT + CA_CL +  
  CA_TD + CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA +EBIT_SAL  #v2 paper


stargazer(data.frame(V1_names), summary= F)


V1=glm(pred_default1 ~ AT + ACT + AO + CAPX + CHE + COGS + DP + EPSPX + 
         INVT + LCT + LT + NI + PPENT + RE + REVT + SALE + 
         WCAP + XOPR + CEQ + PRCC_C + IBC + XINT + DLC + DVT + EBIT + 
         EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT + 
         CSHO + MKVALT + MKTVAL_LT + WC_TA + CSH_FLOW + CSH_HOLD + 
         CSH_FLOW_CL + CSH_FLOW_LT + CA_CL + CHE_CL + CL_E + 
         CA_TD + MKT_TA + CSH_CAPX + CH_OP_MAR + EPS + ROA + ROE + 
         EBIT_TA + EBIT_SAL +  SAL_TA + RE_TA, data = TRAIN, family = "binomial") # V1 = unrestricted model ; ALL






V1_1=glm(V1_names, data = TRAIN, family = "binomial") # V2
alias(V1) 

stargazer(V1_1, type = "latex", summary = FALSE)
v1_VIF=car::vif(V1) # extreme VIF !!

stargazer(v1_VIF,summary = FALSE)
stargazer(t1$byClass,summary = F)

# 
# pred_v1 <- predict(V1, newdata = TEST, type = "response")
# caret::confusionMatrix(data = as.numeric(pred_v1>0.4), reference = TEST$pred_default1)


#>0.065 UNSCALED
pred_v1 <- predict(V1_1, newdata = TEST, type = "response")
pred_v1 <- as.factor(ifelse(predict(V1_1, newdata = TEST, type="response")>0.05,"1","0")) # or 0.1
table(as.factor(pred_v1))
t1=caret::confusionMatrix(data= pred_v1, reference=as.factor(TEST$pred_default1),positive="1")
t1$table
t1_cm= as.data.frame.matrix(t1$table)
t1$byClass
stargazer(t1_cm,summary = FALSE, type = "latex")
#skip for complete ROC :
roc(TEST$pred_default1,as.numeric(pred_v1),plot = T,col="black",print.auc=T)
auc(TEST$pred_default1,as.numeric(pred_v1))



roc(TEST$pred_default1,as.numeric(pred_v1),plot = T)


#variable selection: null nodel
Null_model <- glm(pred_default1 ~ 1, TRAIN,family = "binomial")
V2 <- stepAIC(Null_model, direction="forward", scope=list(lower=Null_model, upper=V1),family="binomial")
# OLD_V2_names = pred_default1 ~ PRCC_C + RE + log(CAPX) + log(CHE) + 
#   NI + IBC + DP + ICAPT + CA_CL + AO + CAPX + EBIT_SAL + EBIT + 
#   AT + LCT + XINT + DLC, data = TRAIN


#V2_names = pred_default1 ~ MKTVAL_LT + CA_CL + CHE_CL + CHE_AT + CL_E + 
  # CA_TD + log(ACT) + log(CAPX) + log(CHE) + log(SALE) + CSH_FLOW_LT + 
  # CSH_CAPX + EPS + ROA + ROE + EBIT_TA + EBIT_SAL

pred_v2 <- predict(V2, newdata = TEST, type = "response")
pred_v2 <- as.factor(ifelse(predict(V2, newdata = TEST, type="response")>0.05,"1","0")) # or 0.1
table(as.factor(pred_v2))
t2=caret::confusionMatrix(data= pred_v2, reference=as.factor(TEST$pred_default1),positive="1")
t2
t2_cm= as.data.frame.matrix(t2$table)


stargazer(V3)
stargazer(V2)

stargazer(t2_cm,summary = F)
roc(TEST$pred_default1,as.numeric(pred_v2),plot = T)
auc(TEST$pred_default1,as.numeric(pred_v2))


V3 <- stepAIC(V1, trace = TRUE, direction= "backward")
# pred_default1 ~ AT + ACT + CAPX + INVT + LT + NI + RE + SALE + 
  #   WCAP + PRCC_C + IBC + XINT + DLC + EBIT + EBITDA + BKVLPS + 
  #   DLTT + DVC + ICAPT + CSHO + WC_TA + CSH_HOLD + CA_CL + CL_E + 
  #   CA_TD + CSH_CAPX + CH_OP_MAR + EPS + ROE + EBIT_TA + EBIT_SAL + 
  #   SAL_TA + RE_TA

V3=glm(pred_default1 ~ AT + ACT + CAPX + INVT + LT + NI + RE + SALE + 
            WCAP + PRCC_C + IBC + XINT + DLC + EBIT + EBITDA + BKVLPS + 
            DLTT + DVC + ICAPT + CSHO + WC_TA + CSH_HOLD + CA_CL + CL_E + 
            CA_TD + CSH_CAPX + CH_OP_MAR + EPS + ROE + EBIT_TA + EBIT_SAL + 
            SAL_TA + RE_TA,data=TRAIN,family = "binomial")

pred_v3 <- predict(V3, newdata = TEST, type = "response")
#pred_v3 <- as.factor(ifelse(pred_v3>0.1,1,0))

pred_v3 <- as.factor(ifelse(predict(V3, newdata = TEST, type="response")>0.06,"1","0")) # or 0.1

t3=caret::confusionMatrix(data= pred_v3, reference=as.factor(TEST$pred_default1),positive="1")
t3_cm= as.data.frame.matrix(t3$table)
# pred_default1 ~ AT + ACT + CAPX + INVT + LT + NI + RE + SALE + 
#   WCAP + PRCC_C + IBC + XINT + DLC + EBIT + EBITDA + BKVLPS + 
#   DLTT + DVC + ICAPT + CSHO + WC_TA + CSH_HOLD + CA_CL + CL_E + 
#   CA_TD + CSH_CAPX + CH_OP_MAR + EPS + ROE + EBIT_TA + EBIT_SAL + 
#   SAL_TA + RE_TA
stargazer(t3_cm,summary = F)

stargazer(V3, summary = F)
stargazer(V3)


roc(TEST$pred_default1,as.numeric(pred_v2),plot = T,col="black",print.auc=T,smooth = T)



roc(TEST$pred_default1,as.numeric(pred_v3),plot = T,col="black",print.auc=T)
auc(TEST$pred_default1,as.numeric(pred_v3))



V4 <- stepAIC(V1, trace = TRUE, direction= "both")
# OLD_V4_names= pred_default1 ~ AT + AO + CAPX + LCT + LT + NI + SALE + PRCC_C + 
#   IBC + XINT + DLC + DVT + EBIT + EBITDA + DLTT + DVP + DVC + 
#   ICAPT + WC_TA + CSH_FLOW + CSH_FLOW_CL + log(CAPX) + log(CHE) + 
#   CSH_FLOW_LT + CSH_CAPX + ROA + ROE 

# pred_default1 ~ log(CAPX) + log(CHE) + CSH_CAPX + EPS + ROA + 
#  ROE + EBIT_TA SAME AS BACKWARD

# both &  AIC, same reuslts as backward's.

pred_v4 <- predict(V4, newdata = TEST, type = "response")
t4=confusionMatrix(as.numeric(pred_v4>0.5),TEST$pred_default1)



roc(TEST$pred_default1,as.numeric(pred_v4),plot = T)
auc(TEST$pred_default1,as.numeric(pred_v4))


stargazer(V1,V2,V3,V4, type = "text")


# log(AT) + log(ACT) + log(CAPX) + log(CHE) + log(COGS) + log(DP) + EPSPX +
#           log(INVT) + log(LCT) + log(LT) + NI + OIBDP + log(PPENT) + RE + log(REVT) + log(SALE) +
#           WCAP + log(XOPR) + CEQ + PRCC_C + IBC + log(XINT) + DLC + DVT + EBIT +
#          EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT + CSHO +
#           BV + CSH_FLOW + LVRG + MKVALT + MTB +
#           ROE + WC_TA + RE_AT + CA_CL + CH_CL + CH_AT + CSH_FLOW_CL +
#           CSH_FLOW_LT + CL_E + CA_TD + QA_CL + QA_INV + WC_SAL + INV_SA +
#           COGS_INV + COGS_SAL + NI_TA + NI_E + NI_SAL + EBIT_INVCAP + EBIT_TA +
#           EBIT_SAL + SAL_TA , data= TRAIN)


# a vif score over 5 is a problem. A score over 10 should be remedied 
# and you should consider dropping the problematic variable
# from the regression model or creating an index of all the closely related variables.

#REGULARIZED MODELS : 

 V1_names= pred_default1 ~ AT + ACT + AO + CAPX + CHE + COGS + DP + EPSPX +
    INVT + LCT + LT + NI + PPENT + RE + REVT +
    WCAP + XOPR + CEQ + PRCC_C + XINT + DLC + DVT + EBIT +
    EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT+
    CSHO + MKVALT + MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
    CL_E + CA_TD +
    CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA 
 

# Predictor variables
x <- model.matrix(V1_names,y= TRAIN$pred_default1, data=TRAIN)[,-1]

# Outcome variable
y <- TRAIN$pred_default1

glmnet(x, y, alpha = 1, lambda = NULL)

# ALPHA =0 :“0”: for ridge regression
# ALPHA =1 :  1”: for lasso regression
# aLPHA VALUES  value between 0 and 1 for elastic net regression.

# lamba: a numeric value defining the amount of shrinkage.
#lambda that minimize the cross-validation prediction error rate : 


cv1 <- cv.glmnet(x, y,family = "binomial", alpha = 0)
plot(cv1)


cv2 <- cv.glmnet(x, y,family = "binomial", alpha = 1)
plot(cv2)

cv1$lambda.min
cv2$lambda.min


x.TEST <- model.matrix(V1_names,y= pred_default1, data=TEST)[,-1]

x.TRAIN <- model.matrix(V1_names,y= pred_default1, data=TRAIN)[,-1]

RIDGE <- glmnet(x, y, alpha = 0, lambda = cv1$lambda.min)
LASSO <- glmnet(x, y, alpha = 1, lambda = cv2$lambda.min)

ta_RI=data.frame(coef(RIDGE))
stargazer(ta_RI, summary = F)

coef(LASSO)

prob_RIDGE <-RIDGE %>% predict(x.TEST)
prob_LASSO <-LASSO %>% predict(x.TEST)

pred_RIDGE <- factor(ifelse(prob_RIDGE > 0.05, "1", "0"))
pred_LASSO<- factor(ifelse(prob_LASSO > 0.0463, "1", "0"))


RIDGE_cm=caret::confusionMatrix(pred_RIDGE,factor(TEST$pred_default1), positive= "1")
LASSO_cm=caret::confusionMatrix(pred_LASSO,factor(TEST$pred_default1), positive= "1")


roc(TEST$pred_default1,as.numeric(prob_LASSO),plot = T,col="black",print.auc=T)
roc(TEST$pred_default1,as.numeric(prob_RIDGE),plot = T,col="black",print.auc=T)

# par(mfrow = c(1, 2))
# plot(glmnet(x, y, family = "binomial", alpha = 1))
# plot(glmnet(x, y, family = "binomial"), xvar = "lambda")

RIDGE_cm
LASSO_cm


#Elastic net :
TRAIN$pred_default1 =factor(TRAIN$pred_default1)
EN <- train(V1_names, data = TRAIN, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
EN
# Best tuning parameter
EN$bestTune

ELAST_NET <- glmnet(x, y, alpha = EN$bestTune$alpha, lambda = EN$bestTune$lambda, family = "binomial")
coef(ELAST_NET)
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were alpha = 0.9 and lambda = 0.01259045.


prob_EN<-ELAST_NET %>% predict(x.TEST)
pred_EN<- factor(ifelse(prob_EN > -3, "1", "0"))

table(pred_EN)

EN_cm=caret::confusionMatrix(pred_EN,factor(TEST$pred_default1), positive= "1")
EN_cm

assess.glmnet(ELAST_NET, newx = x.TEST, newy = TEST$pred_default1)

plot(roc.glmnet(ELAST_NET, newx = x.TEST, newy = TEST$pred_default1))

roc(TEST$pred_default1,prob_EN,plot=T,auc = T)

ROC_auto(ELAST_NET,TRAIN,TEST)
roc(TEST$pred_default1,pred_EN,plot = T)




##-------------------treees
TRAIN
library(rpart)
library(rpart.plot)
fit <- rpart(pred_default1 ~ AT + ACT + AO + CAPX + CHE + COGS + DP + EPSPX +
                INVT + LCT + LT + NI + PPENT + RE + REVT +
                WCAP + XOPR + CEQ + PRCC_C + XINT + DLC + DVT + EBIT +
                EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT+
                CSHO + MKVALT + MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
                CL_E + CA_TD +
                CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA , data = TRAIN, method = 'class')
rpart.plot(fit, extra = 104)

rpart.rules(fit)

pred <- predict(fit, newdata=TEST)
roc(factor(TEST$pred_default1),pred,plot=T)

# neural network : ---------------------------------






V1_names= pred_default1 ~ AT + ACT + AO + CAPX + CHE + COGS + DP + EPSPX +
  INVT + LCT + LT + NI + PPENT + RE + REVT +
  WCAP + XOPR + CEQ + PRCC_C + XINT + DLC + DVT + EBIT +
  EBITDA + BKVLPS + DLTT + SEQ + DVP + DVC + PRSTKC + ICAPT+
  CSHO + MKVALT + MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
  CL_E + CA_TD +
  CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA 



V1_names= pred_default1 ~ MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
  CL_E + CA_TD +  CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA 


# TREES : 

library(party)

tree1=ctree(pred_default1 ~ MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
        CL_E + CA_TD +  CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA 
      , TRAIN)

plot(tree1)



# decision trees :


r1=rpart(V1_names, data=TRAIN, method="class")
plot(r1)
text(r1)







#install.packages("rpart.plot")	
library(rpart.plot)


V1_names= pred_default1 ~ MKTVAL_LT + WC_TA + CSH_FLOW + CSH_FLOW_CL + CA_CL + CHE_CL + CHE_AT +
  CL_E + CA_TD + CSH_FLOW_LT + CSH_CAPX + EPS + ROA + ROE + EBIT_TA 


fit=rpart(V1_names, data=TEST, method=	"class") 

# A single tree :
rpart.plot(fit, extra= 104)

# predictecd class
# % of 1/0 in the leaf. => create future probability of classification 
# % of obs present in the leaf.

plot(fit)
text(fit)
# summary(fit)	

# -----------------------------------
library(randomForest)




y=factor(TEST$pred_default1)

randomForest(V1_names,DATA=V1_names, y=NULL,  xtest=TEST$, ytest=TEST$pred_default1, ntree=500)
randomForest(V1_names,DATA=V1_names, y=NULL,  xtest=NULL, ytest=NULL, ntree=500,)




fit2 <- randomForest(V1_names,y, data=TEST)
print(fit2) # view results
importance(fit2) # importance of each predictor

# 3586 22 
# 161  0 

library(xgboost)

model <- xgboost(data = TRAIN, label = TRAIN,
                 nrounds = 2, objective = "binary:logistic")

# nround= number of decision trees
# objective = "binary:logistic" : binary classification

#despite the appearences (...)



