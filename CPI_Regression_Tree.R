library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(scales)
cpi = fread('C:/Users/anany/OneDrive - Nanyang Technological University/Desktop/CPI_Data_Edited_Final.csv')

###brazil

set.seed(2000)
cpi_brazil = cpi[country == "Brazil", ]
cpi_brazil[, c('country','year') := NULL]



#split train and test set
train_cpi_brazil = sample.split(Y = cpi_brazil$`CPI`, SplitRatio = 0.8)
trainset_cpi_brazil = subset(cpi_brazil, train_cpi_brazil == T)
testset_cpi_brazil = subset(cpi_brazil, train_cpi_brazil == F)


#grow the tree
cart_cpi_brazil = rpart(`CPI` ~ ., data = trainset_cpi_brazil, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_cpi_brazil)
print(cart_cpi_brazil)
plotcp(cart_cpi_brazil)
summary(cart_cpi_brazil)


CVerror.cap.cpi_brazil = cart_cpi_brazil$cptable[which.min(cart_cpi_brazil$cptable[,"xerror"]), "xerror"] + cart_cpi_brazil$cptable[which.min(cart_cpi_brazil$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_cpi_brazil$cptable[i,j] > CVerror.cap.cpi_brazil) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_cpi_brazil = ifelse(i > 1, sqrt(cart_cpi_brazil$cptable[i,1] * cart_cpi_brazil$cptable[i-1,1]), 1)

#prune the tree
cart_cpi_brazil_prune = prune(cart_cpi_brazil, cp = cp_cpi_brazil)
printcp(cart_cpi_brazil_prune)
rpart.plot(cart_cpi_brazil_prune, nn= T, main = "Pruned Tree Brazil CPI")



#Test data
cart_cpi_brazil_predict = predict(cart_cpi_brazil_prune, newdata = testset_cpi_brazil)
actual_cpi_brazil = testset_cpi_brazil$`CPI`

#MSE
mean((cart_cpi_brazil_predict-actual_cpi_brazil)^2)

#MAPE
cpi_brazil_error = cart_cpi_brazil_predict - actual_cpi_brazil
cpi_brazil_error_percentage = percent(cpi_brazil_error / actual_cpi_brazil,
                                      accuracy = .01,suffix="%")

results.cpi_brazil = data.table(actual_cpi_brazil, cart_cpi_brazil_predict, 
                                cpi_brazil_error, cpi_brazil_error_percentage)
colnames(results.cpi_brazil) = c("Actual CPI", "Model Predicted CPI",
                                 "Testset Error", "Testset Error Percentage")
abstesterror.cpi_brazil = abs(cpi_brazil_error)
MAPE.cpi_brazil = mean(abstesterror.cpi_brazil / actual_cpi_brazil)*100


###US

set.seed(2000)
cpi_US = cpi[country == "United States", ]
cpi_US[, c('country','year') := NULL]


#split train and test set
train_cpi_US = sample.split(Y = cpi_US$`CPI`, SplitRatio = 0.8)
trainset_cpi_US = subset(cpi_US, train_cpi_US == T)
testset_cpi_US = subset(cpi_US, train_cpi_US == F)

#grow the tree
cart_cpi_US = rpart(`CPI` ~ ., data = trainset_cpi_US, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_cpi_US)
print(cart_cpi_US)
plotcp(cart_cpi_US)
summary(cart_cpi_US)


CVerror.cap.cpi_US = cart_cpi_US$cptable[which.min(cart_cpi_US$cptable[,"xerror"]), "xerror"] + cart_cpi_US$cptable[which.min(cart_cpi_US$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_cpi_US$cptable[i,j] > CVerror.cap.cpi_US) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_cpi_US = ifelse(i > 1, sqrt(cart_cpi_US$cptable[i,1] * cart_cpi_US$cptable[i-1,1]), 1)

#prune the tree
cart_cpi_US_prune = prune(cart_cpi_US, cp = cp_cpi_US)
printcp(cart_cpi_US_prune)
rpart.plot(cart_cpi_US_prune, nn= T, main = "Pruned Tree US CPI")


#Test data
cart_cpi_US_predict = predict(cart_cpi_US_prune, newdata = testset_cpi_US)
actual_cpi_US = testset_cpi_US$`CPI`

#MSE
mean((cart_cpi_US_predict-actual_cpi_US)^2)

#MAPE
cpi_US_error = cart_cpi_US_predict - actual_cpi_US
cpi_US_error_percentage = percent(cpi_US_error / actual_cpi_US,
                                  accuracy = .01,suffix="%")

results.cpi_US = data.table(actual_cpi_US, cart_cpi_US_predict, 
                            cpi_US_error, cpi_US_error_percentage)
colnames(results.cpi_US) = c("Actual CPI", "Model Predicted CPI",
                             "Testset Error", "Testset Error Percentage")
abstesterror.cpi_US = abs(cpi_US_error)
MAPE.cpi_US = mean(abstesterror.cpi_US / actual_cpi_US)*100



###Vietnam
set.seed(2000)
cpi_vietnam = cpi[country == "Vietnam", ]
cpi_vietnam[, c('country','year') := NULL]


#split train and test set
train_cpi_vietnam = sample.split(Y = cpi_vietnam$`CPI`, SplitRatio = 0.8)
trainset_cpi_vietnam = subset(cpi_vietnam, train_cpi_vietnam == T)
testset_cpi_vietnam = subset(cpi_vietnam, train_cpi_vietnam == F)

#grow the tree
cart_cpi_vietnam = rpart(`CPI` ~ ., data = trainset_cpi_vietnam, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(cart_cpi_vietnam)
print(cart_cpi_vietnam)
plotcp(cart_cpi_vietnam)
summary(cart_cpi_vietnam)


CVerror.cap.cpi_vietnam = cart_cpi_vietnam$cptable[which.min(cart_cpi_vietnam$cptable[,"xerror"]), "xerror"] + cart_cpi_vietnam$cptable[which.min(cart_cpi_vietnam$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_cpi_vietnam$cptable[i,j] > CVerror.cap.cpi_vietnam) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_cpi_vietnam = ifelse(i > 1, sqrt(cart_cpi_vietnam$cptable[i,1] * cart_cpi_vietnam$cptable[i-1,1]), 1)

#prune the tree
cart_cpi_vietnam_prune = prune(cart_cpi_vietnam, cp = cp_cpi_vietnam)
printcp(cart_cpi_vietnam_prune)
rpart.plot(cart_cpi_vietnam_prune, nn= T, main = "Pruned Tree Vietnam CPI")


#Test data
cart_cpi_vietnam_predict = predict(cart_cpi_vietnam_prune, newdata = testset_cpi_vietnam)
actual_cpi_vietnam = testset_cpi_vietnam$`CPI`

#MSE
mean((cart_cpi_vietnam_predict-actual_cpi_vietnam)^2)

#MAPE
cpi_vietnam_error = cart_cpi_vietnam_predict - actual_cpi_vietnam
cpi_vietnam_error_percentage = percent(cpi_vietnam_error / actual_cpi_vietnam,
                                       accuracy = .01,suffix="%")

results.cpi_vietnam = data.table(actual_cpi_vietnam, cart_cpi_vietnam_predict, 
                                 cpi_vietnam_error, cpi_vietnam_error_percentage)
colnames(results.cpi_vietnam) = c("Actual CPI", "Model Predicted CPI",
                                  "Testset Error", "Testset Error Percentage")
abstesterror.cpi_vietnam = abs(cpi_vietnam_error)
MAPE.cpi_vietnam = mean(abstesterror.cpi_vietnam / actual_cpi_vietnam)*100

