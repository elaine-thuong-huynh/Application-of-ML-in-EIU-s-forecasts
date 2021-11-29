library(car)
library(corrplot)
library(caTools)
library(data.table)
library(scales)
library(ggplot2)


CPI_data <- fread("CPI_Dataset.csv")

-----------------------------------------------------------------------------------------------------

## Full 3 Countries
## Full Linear Model & Backward Elimination
m.full <- lm(CPI ~ .-country, data = CPI_data)
summary(m.full) 
step.full <- step(m.full)
summary(step.full)

## Diagnostic Plots
par(mfrow = c(2,2)) # Plot 4 charts in one plot - 2 by 2.
plot(step.full)  # Plot model 4 diagnostics. Results are good for all 4 plots
par(mfrow = c(1,1)) # Reset plot options to 1 chart in one plot.

# VIF to check for multi-colinearity
vif(m.full)
vif(step.full)

# Generate a random number sequence that can be reproduced to verify results.
# Train-Test Split based on random split 80-20
set.seed(2000)
train.full.seed <- sample.split(Y = CPI_data$CPI, SplitRatio = 0.8)
train.full <- subset(CPI_data, train.full.seed == T)
test.full <- subset(CPI_data, train.full.seed == F)

# Develop model on trainset
m.train.full <- lm(CPI ~ .-country, data = train.full)
summary(m.train.full) 
step.train.full <- step(m.train.full)
summary(step.train.full) 
residuals(step.train.full) 

# Diagnostic Plots for Train Model
par(mfrow = c(2,2)) 
plot(step.train.full)

# Residuals = Error = Actual CPI - Model Predicted CPI
RMSE.full.train <- sqrt(mean(residuals(step.train.full)^2))  
# RMSE on trainset based on step.train.full model.
summary(abs(residuals(step.train.full)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.full.test <- predict(step.train.full, newdata = test.full)
testset.full.error <- test.full$CPI - predict.full.test

# Testset Errors
RMSE.full.test <- sqrt(mean(testset.full.error^2))
summary(abs(testset.full.error))

# Projecting Results of model predicted CPI vs actual CPI on a Table
testcases.full <- test.full[,c(1,2)]
testset.full.error.percentage <- percent(c(testset.full.error) / c(test.full$CPI),
                                        accuracy = .01,suffix="%")
abstesterror.full <- abs(testset.full.error)
MAPE.full <- percent(mean(abstesterror.full/test.full$CPI), accuracy = .01, suffix = "%")
Results.full.random <- data.frame(testcases.full, test.full$CPI, predict.full.test, testset.full.error, 
                               testset.full.error.percentage)
colnames(Results.full.random) <- c("country","year","Actual CPI", "Model Predicted CPI",
                                "Testset Error", "Testset Error Percentage")
MAPE.full
View(Results.full.random)

------------------------------------------------------------------------------------------------------

# USA Dataset
USA_CPI <- CPI_data[country=="United States"]

## USA Data Exploration
summary(USA_CPI)
corr_USA <- fread("USA_Correlation.csv")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor(corr_USA), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of USA Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

## Full Linear Model & Backward Elimination
m.usa <- lm(CPI ~ year + PPI + Share_Prices+ Interest_Rate + GDP_per_capita + Exchange_Rate 
            + Unemployment + Expenditure + Manufacturing, data = USA_CPI)
summary(m.usa) 
step.usa <- step(m.usa)
summary(step.usa)

# Diagnostic Plots
par(mfrow = c(2,2)) 
plot(step.usa)  
par(mfrow = c(1,1)) 

# VIF to check for multi-colinearity
vif(m.usa)
vif(step.usa)

# Generate a random number sequence that can be reproduced to verify results.
# Train-Test Split based on random split 80-20
set.seed(2000)
train.usa.seed <- sample.split(Y = USA_CPI$CPI, SplitRatio = 0.8)
train.usa <- subset(USA_CPI, train.usa.seed == T)
test.usa <- subset(USA_CPI, train.usa.seed == F)


# Develop model on trainset
m.train.usa <- lm(CPI ~ year + PPI + Share_Prices + Interest_Rate + GDP_per_capita + Exchange_Rate 
                  + Unemployment + Expenditure + Manufacturing, data = train.usa)
summary(m.train.usa) 
step.train.usa <- step(m.train.usa)
summary(step.train.usa) 
residuals(step.train.usa) 

# Train Model Multicollinearity
vif(m.train.usa)
vif(step.train.usa)

# Train Model Diagnostic Plots
par(mfrow = c(2,2)) 
plot(step.train.usa)

# Residuals = Error = Actual CPI - Model Predicted CPI
RMSE.usa.train <- sqrt(mean(residuals(step.train.usa)^2))  
# RMSE on trainset based on step.train.usa model.
summary(abs(residuals(step.train.usa)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.usa.test <- predict(step.train.usa, newdata = test.usa)
testset.usa.error <- test.usa$CPI - predict.usa.test

# Testset Errors
RMSE.usa.test <- sqrt(mean(testset.usa.error^2))
mean(abs(testset.usa.error))
RMSE.usa.test

# Projecting Results of model predicted CPI vs actual CPI on a Table
testcases.usa <- test.usa[,c(1,2)]
testset.usa.error.percentage <- percent(c(testset.usa.error) / c(test.usa$CPI),
                                        accuracy = .01,suffix="%")
abstesterror.usa <- abs(testset.usa.error)
MAPE.usa <- percent(mean(abstesterror.usa/test.usa$CPI), accuracy = .01, suffix = "%")
Results.USA.random <- data.frame(testcases.usa, test.usa$CPI, predict.usa.test, testset.usa.error, 
                               testset.usa.error.percentage)
colnames(Results.USA.random) <- c("country","year","Actual CPI", "Model Predicted CPI",
                                "Testset Error", "Testset Error Percentage")
MAPE.usa
View(Results.USA.random)

# Comparison with EIU's accuracy
# Train-Test split by year with testset from 2016 to 2019
train.usa.year <- USA_CPI[year<2016]
test.usa.year <- USA_CPI[year>=2016]

# Develop Model based on new train-test split
m.train.usa3 <- lm(CPI ~ year + PPI + Share_Prices + Interest_Rate + GDP_per_capita + Exchange_Rate 
                  + Unemployment + Expenditure + Manufacturing, data = train.usa.year)
summary(m.train.usa3) 
step.train.usa3 <- step(m.train.usa3)
summary(step.train.usa3) 

# Apply train model on testset to predict new values
predict.usa.test3 <- predict(step.train.usa3, newdata = test.usa.year)
testset.usa.error3 <- test.usa.year$CPI - predict.usa.test3

# Calculate RMSE & MAE
RMSE.usa.test3 <- sqrt(mean(testset.usa.error3^2))
mean(abs(testset.usa.error3))
RMSE.usa.test3

# Projecting Results
testcases.usa3 <- test.usa.year[,2]
testset.usa.error.percentage3 <- percent(c(testset.usa.error3) / c(test.usa.year$CPI),
                                        accuracy = .01,suffix="%")
abstesterror.usa3 <- abs(testset.usa.error3)
MAPE.usa.year <- percent(mean(abstesterror.usa3/test.usa.year$CPI), accuracy = .01, suffix = "%")
Results.USA.year <- data.frame(testcases.usa3, test.usa.year$CPI, predict.usa.test3, testset.usa.error3, 
                                 testset.usa.error.percentage3)
colnames(Results.USA.year) <- c("year","Actual CPI", "Model Predicted CPI",
                                  "Testset Error", "Testset Error Percentage")
MAPE.usa.year
View(Results.USA.year)


# Proof of Concept (POC)
# Prediction of 2022 USA CPI
USA_CPI_2022 <- fread("USA_CPI_2022.csv")

# Apply train model to predict 2022 CPI
predict.usa.test2 <- predict(step.usa, newdata = USA_CPI_2022)

# Results of predicted 2022 CPI
testcases.usa2 <- USA_CPI_2022[,c(1,2)]
Results.USA.2022 <- data.frame(testcases.usa2, predict.usa.test2)
colnames(Results.USA.2022) <- c("country","year","Model Predicted CPI")
View(Results.USA.2022)

-------------------------------------------------------------------------------------------------

#Brazil Dataset
Brazil_CPI <- CPI_data[country=="Brazil"]

# Data Exploration
summary(Brazil_CPI)
corr_Brazil <- fread("Brazil_Correlation.csv")

corrplot(cor(corr_Brazil), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Brazil Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

## Full Linear Model & Backward Elimination
m.brazil <- lm(CPI ~ year + PPI + Share_Prices+ Interest_Rate + GDP_per_capita + Exchange_Rate 
               + Unemployment + Expenditure + Manufacturing, data = Brazil_CPI)
summary(m.brazil) 
step.brazil <- step(m.brazil)
summary(step.brazil)

# Diagnostic Plots
par(mfrow = c(2,2)) 
plot(step.brazil)  # Plot model 4 diagnostics. Results are good for all 4 plots
par(mfrow = c(1,1)) 

# VIF to check for multi-colinearity
vif(m.brazil)
vif(step.brazil)

# Generate a random number sequence that can be reproduced to verify results.
# Train-Test Split based on random split 80-20
set.seed(2000)
train.brazil.seed <- sample.split(Y = Brazil_CPI$CPI, SplitRatio = 0.8)
train.brazil <- subset(Brazil_CPI, train.brazil.seed == T)
test.brazil <- subset(Brazil_CPI, train.brazil.seed == F)

# Develop model on trainset
m.train.brazil <- lm(CPI ~ year + PPI + Share_Prices+ Interest_Rate + GDP_per_capita + Exchange_Rate 
                     + Unemployment + Expenditure + Manufacturing, data = train.brazil)
summary(m.train.brazil) 
step.train.brazil <- step(m.train.brazil)
summary(step.train.brazil) 
residuals(step.train.brazil) 

# Diagnostic Plots
par(mfrow = c(2,2)) # Plot 4 charts in one plot - 2 by 2.
plot(step.train.brazil)

# Train Model Multicolinearity
vif(m.train.brazil)
vif(step.train.brazil)

# Residuals = Error = Actual CPI - Model Predicted CPI
RMSE.brazil.train <- sqrt(mean(residuals(step.train.brazil)^2))  
# RMSE on trainset based on step.train.brazil model.
summary(abs(residuals(step.train.brazil)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.brazil.test <- predict(step.train.brazil, newdata = test.brazil)
testset.brazil.error <- test.brazil$CPI - predict.brazil.test

# Testset Errors
RMSE.brazil.test <- sqrt(mean(testset.brazil.error^2))
mean(abs(testset.brazil.error))
RMSE.brazil.test

# Projecting Results of model predicted CPI vs actual CPI on a Table
testcases.brazil <- test.brazil[,c(1,2)]
testset.brazil.error.percentage <- percent(c(testset.brazil.error) / c(test.brazil$CPI),
                                        accuracy = .01,suffix="%")
abstesterror.brazil <- abs(testset.brazil.error)
MAPE.brazil <- percent(mean(abstesterror.brazil/test.brazil$CPI),accuracy =.01, suffix = "%")
Results.brazil.random <- data.frame(testcases.brazil, test.brazil$CPI, predict.brazil.test, testset.brazil.error, 
                               testset.brazil.error.percentage)
colnames(Results.brazil.random) <- c("country","year","Actual CPI", "Model Predicted CPI",
                                "Testset Error", "Testset Error Percentage")
MAPE.brazil
View(Results.brazil.random)

# Comparison with EIU's accuracy
# Train-Test split by year with testset from 2016 to 2019
train.brazil.year <- Brazil_CPI[year<2016]
test.brazil.year <- Brazil_CPI[year>=2016]

# Develop Model based on new train-test split
m.train.brazil3 <- lm(CPI ~ year + PPI + Share_Prices + Interest_Rate + GDP_per_capita + Exchange_Rate 
                       + Unemployment + Expenditure + Manufacturing, data = train.brazil.year)
summary(m.train.brazil3) 
step.train.brazil3 <- step(m.train.brazil3)
summary(step.train.brazil3) 

# Apply train model on testset to predict new values
predict.brazil.test3 <- predict(step.train.brazil3, newdata = test.brazil.year)
testset.brazil.error3 <- test.brazil.year$CPI - predict.brazil.test3

# Calculate RMSE & MAE
RMSE.brazil.test3 <- sqrt(mean(testset.brazil.error3^2))
mean(abs(testset.brazil.error3))
RMSE.brazil.test3

# Projecting Results
testcases.brazil3 <- test.brazil.year[,2]
testset.brazil.error.percentage3 <- percent(c(testset.brazil.error3) / c(test.brazil.year$CPI),
                                             accuracy = .01,suffix="%")
abstesterror.brazil3 <- abs(testset.brazil.error3)
MAPE.brazil.year <- percent(mean(abstesterror.brazil3/test.brazil.year$CPI), accuracy = .01, suffix = "%")
Results.brazil.year <- data.frame(testcases.brazil3, test.brazil.year$CPI, predict.brazil.test3, testset.brazil.error3, 
                                   testset.brazil.error.percentage3)
colnames(Results.brazil.year) <- c("year","Actual CPI", "Model Predicted CPI",
                                    "Testset Error", "Testset Error Percentage")
MAPE.brazil.year
View(Results.brazil.year)

# Proof of Concept (POC)
# Prediction of 2022 Brazil CPI
Brazil_CPI_2022 <- fread("Brazil_CPI_2022.csv")

# Apply train model to predict 2022 CPI
predict.Brazil.test2 <- predict(step.brazil, newdata = Brazil_CPI_2022)

# Results of predicted 2022 CPI
testcases.Brazil2 <- Brazil_CPI_2022[,c(1,2)]
Results.Brazil.2022 <- data.frame(testcases.Brazil2, predict.Brazil.test2)
colnames(Results.Brazil.2022) <- c("country","year","Model Predicted CPI")
View(Results.Brazil.2022)

--------------------------------------------------------------------------------------------------

  
#Vietnam Dataset
Vietnam_CPI <- CPI_data[country=="Vietnam"]

## Data Exploration
summary(Vietnam_CPI)
corr_Vietnam <- fread("Vietnam_Correlation.csv")

corrplot(cor(corr_Vietnam), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Vietnam Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

## Full Linear Model & Backward Elimination
m.vietnam <- lm(CPI ~ year + PPI + Share_Prices+ Interest_Rate + GDP_per_capita + Exchange_Rate 
                + Unemployment + Expenditure + Manufacturing, data = Vietnam_CPI)
summary(m.vietnam) 
step.vietnam <- step(m.vietnam)
summary(step.vietnam)

# Diagnostic Plots
par(mfrow = c(2,2)) 
plot(step.vietnam)  # Plot model 4 diagnostics. Results are good for all 4 plots
par(mfrow = c(1,1)) 

# VIF to check for multi-colinearity
vif(m.vietnam)
vif(step.vietnam)

# Generate a random number sequence that can be reproduced to verify results.
# Train-Test Split based on random split 80-20
set.seed(2000)
train.vietnam.seed <- sample.split(Y = Vietnam_CPI$CPI, SplitRatio = 0.8)
train.vietnam <- subset(Vietnam_CPI, train.vietnam.seed == T)
test.vietnam <- subset(Vietnam_CPI, train.vietnam.seed == F)

train.vietnam <- Vietnam_CPI[year < 2016]
test.vietnam <- Vietnam_CPI[year >= 2016]

# Develop model on trainset
m.train.vietnam <- lm(CPI ~ year + PPI + Share_Prices+ Interest_Rate + GDP_per_capita + Exchange_Rate 
                      + Unemployment + Expenditure + Manufacturing, data = train.vietnam)
summary(m.train.vietnam) 
step.train.vietnam <- step(m.train.vietnam)
summary(step.train.vietnam)
residuals(step.train.vietnam) 

# Train Model multicolinearity
vif(m.train.vietnam)
vif(step.train.vietnam)

# Train Model Diagnostic Plots
par(mfrow = c(2,2)) # Plot 4 charts in one plot - 2 by 2.
plot(step.train.vietnam)

# Residuals = Error = Actual CPI - Model Predicted CPI
RMSE.vietnam.train <- sqrt(mean(residuals(step.train.vietnam)^2))  
# RMSE on trainset based on step.train.vietnam model.
summary(abs(residuals(step.train.vietnam)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.vietnam.test <- predict(step.train.vietnam, newdata = test.vietnam)
testset.vietnam.error <- test.vietnam$CPI - predict.vietnam.test

# Testset Errors
RMSE.vietnam.test <- sqrt(mean(testset.vietnam.error^2))
mean(abs(testset.vietnam.error))
RMSE.vietnam.test

# Projecting Results of model predicted CPI vs actual CPI on a Table
testcases.vietnam <- test.vietnam[,c(1,2)]
testset.vietnam.error.percentage <- percent(c(testset.vietnam.error) / c(test.vietnam$CPI),
                                           accuracy = .01,suffix="%")
abstesterror.vietnam <- abs(testset.vietnam.error)
MAPE.vietnam <- percent(mean(abstesterror.vietnam/test.vietnam$CPI),accuracy = .01, suffix = "%")
Results.vietnam.random <- data.frame(testcases.vietnam, test.vietnam$CPI, predict.vietnam.test, testset.vietnam.error, 
                                    testset.vietnam.error.percentage)
colnames(Results.vietnam.random) <- c("country","year","Actual CPI", "Model Predicted CPI",
                                     "Testset Error", "Testset Error Percentage")
MAPE.vietnam
View(Results.vietnam.random)

# Comparison with EIU's accuracy
# Train-Test split by year with testset from 2016 to 2019
train.vietnam.year <- Vietnam_CPI[year<2016]
test.vietnam.year <- Vietnam_CPI[year>=2016]

# Develop Model based on new train-test split
m.train.vietnam3 <- lm(CPI ~ year + PPI + Share_Prices + Interest_Rate + GDP_per_capita + Exchange_Rate 
                   + Unemployment + Expenditure + Manufacturing, data = train.vietnam.year)
summary(m.train.vietnam3) 
step.train.vietnam3 <- step(m.train.vietnam3)
summary(step.train.vietnam3) 

# Apply train model on testset to predict new values
predict.vietnam.test3 <- predict(step.train.vietnam3, newdata = test.vietnam.year)
testset.vietnam.error3 <- test.vietnam.year$CPI - predict.vietnam.test3

# Calculate RMSE & MAE
RMSE.vietnam.test3 <- sqrt(mean(testset.vietnam.error3^2))
mean(abs(testset.vietnam.error3))
RMSE.vietnam.test3

# Projecting Results
testcases.vietnam3 <- test.vietnam.year[,2]
testset.vietnam.error.percentage3 <- percent(c(testset.vietnam.error3) / c(test.vietnam.year$CPI),
                                         accuracy = .01,suffix="%")
abstesterror.vietnam3 <- abs(testset.vietnam.error3)
MAPE.vietnam.year <- percent(mean(abstesterror.vietnam3/test.vietnam.year$CPI), accuracy = .01, suffix = "%")
Results.vietnam.year <- data.frame(testcases.vietnam3, test.vietnam.year$CPI, predict.vietnam.test3, testset.vietnam.error3, 
                               testset.vietnam.error.percentage3)
colnames(Results.vietnam.year) <- c("year","Actual CPI", "Model Predicted CPI",
                                "Testset Error", "Testset Error Percentage")
MAPE.vietnam.year
View(Results.vietnam.year)

# Proof of Concept (POC)
# Prediction of 2022 Vietnam CPI
Vietnam_CPI_2022 <- fread("Vietnam_CPI_2022.csv")

# Apply train model to predict 2022 CPI
predict.Vietnam.test2 <- predict(step.train.vietnam, newdata = Vietnam_CPI_2022)

# Results of predicted 2022 CPI
testcases.Vietnam2 <- Vietnam_CPI_2022[,c(1,2)]
Results.Vietnam.2022 <- data.frame(testcases.Vietnam2, predict.Vietnam.test2)
colnames(Results.Vietnam.2022) <- c("country","year","Model Predicted CPI")
View(Results.Vietnam.2022)


--------------------------------"The End"-----------------------------------------------------------
  