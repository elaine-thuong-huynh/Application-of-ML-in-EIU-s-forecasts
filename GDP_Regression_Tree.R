library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(scales)


setwd('E:/2. Studies/2. Y2/1. Sem 1/4. BC2406/0. Course Materials/AY21 Team Assignment and Project/AY21 Team Assignment and Project/Data/cleaned data/official_dataset')
gdp = fread('GDP40_cleaned.csv')

###US

set.seed(2000)
gdp_US = gdp[country == "United States", ]
#gdp_US[, c('country','year') := NULL]


#split train and test set
train_gdp_US = sample.split(Y = gdp_US$`GDP (billions)`, SplitRatio = 0.8)
trainset_gdp_US = subset(gdp_US, train_gdp_US == T)
testset_gdp_US = subset(gdp_US, train_gdp_US == F)

#grow the tree
cart_gdp_US = rpart(`GDP (billions)` ~ .-year-country, data = trainset_gdp_US, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_gdp_US)
print(cart_gdp_US)
plotcp(cart_gdp_US)
rpart.plot(cart_gdp_US, nn= T, main = "Pruned Tree US GDP")
summary(cart_gdp_US)


CVerror.cap.gdp_US = cart_gdp_US$cptable[which.min(cart_gdp_US$cptable[,"xerror"]), "xerror"] + cart_gdp_US$cptable[which.min(cart_gdp_US$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_gdp_US$cptable[i,j] > CVerror.cap.gdp_US) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_gdp_US = ifelse(i > 1, sqrt(cart_gdp_US$cptable[i,1] * cart_gdp_US$cptable[i-1,1]), 1)

#prune the tree
cart_gdp_US_prune = prune(cart_gdp_US, cp = cp_gdp_US)
print(cart_gdp_US_prune)
printcp(cart_gdp_US_prune)
rpart.plot(cart_gdp_US_prune, nn= T, main = "Pruned Tree US GDP")

#Variable importance
cart_gdp_US$variable.importance
# Scaling Variable Impt so as to rep as percentage impt -----------------------
scaledVarImpt.gdp_US = round(100*cart_gdp_US$variable.importance/sum(cart_gdp_US$variable.importance))
View(scaledVarImpt.gdp_US)

#Test data
cart_gdp_US_predict = predict(cart_gdp_US_prune, newdata = testset_gdp_US)
actual_gdp_US = testset_gdp_US$`GDP (billions)`

#MSE & MAPE
testcases_US = testset_gdp_US[, c(1:2)]
gdp_US_error = actual_gdp_US - cart_gdp_US_predict 
gdp_US_error_percentage = percent(gdp_US_error / actual_gdp_US,
                                  accuracy = .01,suffix="%")
gdp_US_squared_error = (cart_gdp_US_predict-actual_gdp_US)^2

results.gdp_US = data.table(testcases_US,actual_gdp_US, cart_gdp_US_predict, 
                            gdp_US_error, gdp_US_squared_error, gdp_US_error_percentage)
colnames(results.gdp_US) = c("Country","Year","Actual GDP", "Model Predicted GDP",
                             "Testset Error", "Testset Squared Error", "Testset Error Percentage")
abstesterror.gdp_US = abs(gdp_US_error)
MAPE.gdp_US = mean(abstesterror.gdp_US / actual_gdp_US)*100
MSE_gdp_US = mean((cart_gdp_US_predict-actual_gdp_US)^2)
View(results.gdp_US)
MAPE.gdp_US
MSE_gdp_US




#-----------------------------------------------------------------------------------------------------------#


#brazil
set.seed(2000)
gdp_brazil = gdp[country == "Brazil", ]
#gdp_brazil[, c('country','year') := NULL]


#split train and test set
train_gdp_brazil = sample.split(Y = gdp_brazil$`GDP (billions)`, SplitRatio = 0.8)
trainset_gdp_brazil = subset(gdp_brazil, train_gdp_brazil == T)
testset_gdp_brazil = subset(gdp_brazil, train_gdp_brazil == F)


#grow the tree
cart_gdp_brazil = rpart(`GDP (billions)` ~ .-country-year, data = trainset_gdp_brazil, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_gdp_brazil)
print(cart_gdp_brazil)
plotcp(cart_gdp_brazil)
summary(cart_gdp_brazil)


CVerror.cap.gdp_brazil = cart_gdp_brazil$cptable[which.min(cart_gdp_brazil$cptable[,"xerror"]), "xerror"] + cart_gdp_brazil$cptable[which.min(cart_gdp_brazil$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_gdp_brazil$cptable[i,j] > CVerror.cap.gdp_brazil) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_gdp_brazil = ifelse(i > 1, sqrt(cart_gdp_brazil$cptable[i,1] * cart_gdp_brazil$cptable[i-1,1]), 1)

#prune the tree
cart_gdp_brazil_prune = prune(cart_gdp_brazil, cp = cp_gdp_brazil)
print(cart_gdp_brazil_prune)
printcp(cart_gdp_brazil_prune)
rpart.plot(cart_gdp_brazil_prune, nn= T, main = "Pruned Tree Brazil GDP")

#Variable Importance

cart_gdp_brazil$variable.importance
# Scaling Variable Impt so as to rep as percentage impt -----------------------
scaledVarImpt.gdp_brazil = round(100*cart_gdp_brazil$variable.importance/sum(cart_gdp_brazil$variable.importance))
View(scaledVarImpt.gdp_brazil)

#Test data
cart_gdp_brazil_predict = predict(cart_gdp_brazil_prune, newdata = testset_gdp_brazil)
actual_gdp_brazil = testset_gdp_brazil$`GDP (billions)`

#MSE & MAPE
testcases_brazil = testset_gdp_brazil[, c(1:2)]
gdp_brazil_error = actual_gdp_brazil - cart_gdp_brazil_predict 
gdp_brazil_error_percentage = percent(gdp_brazil_error / actual_gdp_brazil,
                                  accuracy = .01,suffix="%")
gdp_brazil_squared_error = (cart_gdp_brazil_predict-actual_gdp_brazil)^2

results.gdp_brazil = data.table(testcases_brazil,actual_gdp_brazil, cart_gdp_brazil_predict, 
                            gdp_brazil_error, gdp_brazil_squared_error, gdp_brazil_error_percentage)
colnames(results.gdp_brazil) = c("Country","Year","Actual GDP", "Model Predicted GDP",
                             "Testset Error", "Testset Squared Error", "Testset Error Percentage")
abstesterror.gdp_brazil = abs(gdp_brazil_error)
MAPE.gdp_brazil = mean(abstesterror.gdp_brazil / actual_gdp_brazil)*100
MSE.gdp_brazil = mean((cart_gdp_brazil_predict-actual_gdp_brazil)^2)
View(results.gdp_brazil)
MAPE.gdp_brazil
MSE.gdp_brazil



#-----------------------------------------------------------------------------------------------------------#



###VN
set.seed(2000)
gdp_vietnam = gdp[country == "Vietnam", ]
#gdp_vietnam[, c('country','year') := NULL]


#split train and test set
train_gdp_vietnam = sample.split(Y = gdp_vietnam$`GDP (billions)`, SplitRatio = 0.8)
trainset_gdp_vietnam = subset(gdp_vietnam, train_gdp_vietnam == T)
testset_gdp_vietnam = subset(gdp_vietnam, train_gdp_vietnam == F)

#grow the tree
cart_gdp_vietnam = rpart(`GDP (billions)` ~ .-year-country, data = trainset_gdp_vietnam, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_gdp_vietnam)
print(cart_gdp_vietnam)
plotcp(cart_gdp_vietnam)
summary(cart_gdp_vietnam)


CVerror.cap.gdp_vietnam = cart_gdp_vietnam$cptable[which.min(cart_gdp_vietnam$cptable[,"xerror"]), "xerror"] + cart_gdp_vietnam$cptable[which.min(cart_gdp_vietnam$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_gdp_vietnam$cptable[i,j] > CVerror.cap.gdp_vietnam) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_gdp_vietnam = ifelse(i > 1, sqrt(cart_gdp_vietnam$cptable[i,1] * cart_gdp_vietnam$cptable[i-1,1]), 1)

#prune the tree
cart_gdp_vietnam_prune = prune(cart_gdp_vietnam, cp = cp_gdp_vietnam)
print(cart_gdp_vietnam_prune)
printcp(cart_gdp_vietnam_prune)
rpart.plot(cart_gdp_vietnam_prune, nn= T, main = "Pruned Tree Vietnam GDP")

#Variable importance
cart_gdp_vietnam$variable.importance
# Scaling Variable Impt so as to rep as percentage impt -----------------------
scaledVarImpt.gdp_vietnam = round(100*cart_gdp_vietnam$variable.importance/sum(cart_gdp_vietnam$variable.importance))
View(scaledVarImpt.gdp_vietnam)

#Test data
cart_gdp_vietnam_predict = predict(cart_gdp_vietnam_prune, newdata = testset_gdp_vietnam)
actual_gdp_vietnam = testset_gdp_vietnam$`GDP (billions)`

#MSE & MAPE
testcases_vietnam = testset_gdp_vietnam[, c(1:2)]
gdp_vietnam_error = actual_gdp_vietnam - cart_gdp_vietnam_predict 
gdp_vietnam_error_percentage = percent(gdp_vietnam_error / actual_gdp_vietnam,
                                  accuracy = .01,suffix="%")
gdp_vietnam_squared_error = (cart_gdp_vietnam_predict-actual_gdp_vietnam)^2

results.gdp_vietnam = data.table(testcases_vietnam, actual_gdp_vietnam, cart_gdp_vietnam_predict, 
                            gdp_vietnam_error, gdp_vietnam_squared_error, gdp_vietnam_error_percentage)
colnames(results.gdp_vietnam) = c("Country","Year","Actual GDP", "Model Predicted GDP",
                             "Testset Error", "Testset Squared Error", "Testset Error Percentage")
abstesterror.gdp_vietnam = abs(gdp_vietnam_error)
MAPE.gdp_vietnam = mean(abstesterror.gdp_vietnam / actual_gdp_vietnam)*100
MSE_gdp_vietnam = mean((cart_gdp_vietnam_predict-actual_gdp_vietnam)^2)
View(results.gdp_vietnam)
MAPE.gdp_vietnam
MSE_gdp_vietnam
