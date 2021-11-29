setwd("/Users/michellechung/Desktop/Y2 Sem 1/BC2406 Analytics 1/Project")

library(data.table)
library(dplyr)
library(tidyr)
library(WDI)
library(caTools)
library(car)
library(corrplot)
library(wbstats)
library(scales)


## United States Data 
GDPdata<-fread("GDP40_cleaned.csv")
GDPdata_USA<-GDPdata[GDPdata$country=="United States"]

# Set seed & 80-20 split ratio 
set.seed(2000)
USA_train <- sample.split(Y = GDPdata_USA$`GDP (billions)`, SplitRatio = 0.8)
USA_trainset <- subset(GDPdata_USA, USA_train == T)
USA_testset <- subset(GDPdata_USA, USA_train == F)

# Develop the model on trainset
m1_USA <- lm(`GDP (billions)` ~ year+Life_expectancy_at_birth+`Real_interest_rate (%)`
             +`Gross_Savings (%)`+`Net_Export (billions)` +`Government_Expenditure (billions)`
             +`Debt (%)`+`Investment (%)`, data = USA_trainset)

#Detailed Information on model
summary(m1_USA)

#To eliminate variables that are not significantly important 
#Increase in adjusted R-square
m2_USA <- step(m1_USA)  
summary(m2_USA)

#Check for multicollinearity 
vif(m2_USA)

#Diagnostic plot: observe if predictions can be improved
par(mfrow = c(2, 2))
plot(m2_USA)

# Apply m2_USA to predict on testset.
predict.m2_USA.test <- predict(m2_USA, newdata = USA_testset)

#projecting results of model in a table
USA_testset$Error <- USA_testset$`GDP (billions)` - predict.m2_USA.test 
USA_testset$Error_Perc = percent(c(USA_testset$Error) / c(USA_testset$`GDP (billions)`),accuracy = .01,suffix="%")
USA.Result <- data.frame(USA_testset$country, USA_testset$year, USA_testset$`GDP (billions)`, predict.m2_USA.test, 
                         USA_testset$Error, USA_testset$Error_Perc)
colnames(USA.Result) <- c("country","year","Actual GDP", "Model Predicted GDP",
                                     "Testset Error", "Testset Error Percentage")

#Test m2_USA Accuracy
MAPE.USA <- round(mean(abs(USA_testset$Error / USA_testset$`GDP (billions)`))*100,2)
RMSE.test.m2_USA <- round(sqrt(mean(USA_testset$Error^2)),2)

MAPE.USA
RMSE.test.m2_USA


##Vietname Data 
GDPdata<-fread("GDP40_cleaned.csv")
GDPdata_VNM<-GDPdata[GDPdata$country=="Vietnam"]

# Set seed & 80-20 split ratio 
set.seed(2000)
VNM_train <- sample.split(Y = GDPdata_VNM$`GDP (billions)`, SplitRatio = 0.8)
VNM_trainset <- subset(GDPdata_VNM, VNM_train == T)
VNM_testset <- subset(GDPdata_VNM, VNM_train == F)

# Develop model on trainset
m1_VNM <- lm(`GDP (billions)` ~ year+Life_expectancy_at_birth+`Real_interest_rate (%)`+`Gross_Savings (%)`+`Net_Export (billions)`
             +`Government_Expenditure (billions)`+`Debt (%)`+`Investment (%)`, data = VNM_trainset)

#Detailed Information on model
summary(m1_VNM)

#To eliminate variables that are not significantly important 
#Increase in adjusted R-square from 0.9885 t0 0.9899
m2_VNM <- step(m1_VNM)
summary(m2_VNM)

#Check for multicollinearity 
vif(m2_VNM)

#Diagnostic plot: observe if predictions can be improved 
par(mfrow = c(2, 2))
plot(m2_VNM)

# Apply m2_VNM predict on testset.
predict.m2_VNM.test <- predict(m2_VNM, newdata = VNM_testset)

#projecting results of model in a table
VNM_testset$Error <- VNM_testset$`GDP (billions)` - predict.m2_VNM.test 
VNM_testset$Error_Perc = percent(c(VNM_testset$Error) / c(VNM_testset$`GDP (billions)`),accuracy = .01,suffix="%")
VNM.Result <- data.frame(VNM_testset$country, VNM_testset$year, VNM_testset$`GDP (billions)`, predict.m2_VNM.test, 
                         VNM_testset$Error, VNM_testset$Error_Perc)
colnames(VNM.Result) <- c("country","year","Actual GDP", "Model Predicted GDP",
                          "Testset Error", "Testset Error Percentage")

#Test m2_USA Accuracy
MAPE.VNM <- round(mean(abs(VNM_testset$Error / VNM_testset$`GDP (billions)`))*100,2)
RMSE.test.m2_VNM <- round(sqrt(mean(VNM_testset$Error^2)),2)

MAPE.VNM
RMSE.test.m2_VNM


## Brazil Data 
GDPdata<-fread("GDP40_cleaned.csv")
GDPdata_BRA<-GDPdata[GDPdata$country=="Brazil"]

# Set seed & 80-20 split ratio 
set.seed(2000)
BRA_train <- sample.split(Y = GDPdata_BRA$`GDP (billions)`, SplitRatio = 0.8)
BRA_trainset <- subset(GDPdata_BRA, BRA_train == T)
BRA_testset <- subset(GDPdata_BRA, BRA_train == F)

# Develop model on trainset
m1_BRA <- lm(`GDP (billions)` ~ year+Life_expectancy_at_birth+`Real_interest_rate (%)`+`Gross_Savings (%)`+`Net_Export (billions)`
             +`Government_Expenditure (billions)`+`Debt (%)`+`Investment (%)`, data = BRA_trainset)

#Detailed information of the model
summary(m1_BRA)

#To eliminate those that are not significantly important 
#Increase in adjusted R-square 
m2_BRA <- step(m1_BRA)
summary(m2_BRA)

#Check for multicollinearity 
vif(m2_BRA)

#Diagnostic plot to observe if predictions can be improved 
par(mfrow = c(2, 2))
plot(m2_BRA)

# Apply m2_BRA from trainset to predict on testset.
predict.m2_BRA.test <- predict(m2_BRA, newdata = BRA_testset)

#projecting results of model predicted GDP vs actual GDP on a table 
BRA_testset$Error <- BRA_testset$`GDP (billions)` - predict.m2_BRA.test 
BRA_testset$Error_Perc = percent(c(BRA_testset$Error) / c(BRA_testset$`GDP (billions)`),accuracy = .01,suffix="%")
BRA.Result <- data.frame(BRA_testset$country, BRA_testset$year, BRA_testset$`GDP (billions)`, predict.m2_BRA.test, 
                         BRA_testset$Error, BRA_testset$Error_Perc)
colnames(BRA.Result) <- c("country","year","Actual GDP", "Model Predicted GDP",
                          "Testset Error", "Testset Error Percentage")


#test the accuracy of the model 
MAPE.BRA <- round(mean(abs(BRA_testset$Error / BRA_testset$`GDP (billions)`))*100,2)
RMSE.test.m2_BRA <- round(sqrt(mean(BRA_testset$Error^2)),2)

MAPE.BRA
RMSE.test.m2_BRA