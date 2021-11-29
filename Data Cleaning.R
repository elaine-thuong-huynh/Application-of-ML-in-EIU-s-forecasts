# ==============================================================================================================
#Required Libraries
install.packages("data.table")
library(data.table)

install.packages("imputeTS")
library(imputeTS)

install.packages("dplyr")
library(dplyr)


# ==============================================================================================================
#Importing datasets
GDP40year <- fread("GDPdata_40years.csv")
CPI <- fread("CPI_Dataset.csv")

#Create subsets for each country in GDP40 dataset
Brazil <- GDP40year[1:40,]
United_states <- GDP40year[41:80]
Vietnam <- GDP40year[81:120]

#Create subsets for each country in CPI dataset
Brazil2 <- CPI[1:20,]
United_states2 <- CPI[21:40]
Vietnam2 <- CPI[41:60]

# ==============================================================================================================
#Data Exploration (for missing values)
View(GDP40year)
summary(GDP40year)
View(CPI)
summary(CPI)

# <GDP40year>
View(Brazil)
View(United_states)
View(Vietnam)

#<CPI>
View(Brazil2)
View(United_states2)
View(Vietnam2)

# <GDP40year>
#Total number of NA values in the dataset and percentgae of dataset
navalues <- sum(is.na(GDP40year))
totaldata = 8 * 120
(navalues / totaldata)*100 #NA values consists of 14.5% of dataset

##Check for percentage of missing values of every column
colMeans(is.na(Brazil[,c(4:11)]))*100
colMeans(is.na(United_states[,c(4:11)]))*100
colMeans(is.na(Vietnam[,c(4:11)]))*100

# <CPI>
##Check for percentage of missing values of every column
colMeans(is.na(Brazil2))*100
colMeans(is.na(United_states2))*100
colMeans(is.na(Vietnam2))*100

# (CPI dataset has no null values so no cleaning of NA values is needed)

##For the columns with NA, check if first row value is NA
BrazilwithNA <- c(colnames(Brazil)[!complete.cases(t(Brazil[1,]))])
BrazilwithNA

USwithNA <- c(colnames(United_states)[!complete.cases(t(United_states[1,]))])
USwithNA

VietnamwithNA <- c(colnames(Vietnam)[!complete.cases(t(Vietnam[1,]))])
VietnamwithNA

# ==============================================================================================================
#Data Cleaning (Handling NA Values)

# <GDP40> 
#Find the mean value of each column that has NA
BrazilwithNA
first = mean(Brazil$Real_interest_rate, na.rm = TRUE, dims = 1)
second = mean(Brazil$Debt, na.rm = TRUE, dims = 1)

#Replace First row with that value
Brazil$Real_interest_rate[1] <- first
Brazil$Debt[1] <- second


View(Brazil)
#----------------------
#Find the mean value of each column that has NA
USwithNA
first = mean(United_states$Debt, na.rm = TRUE, dims = 1)

#Replace First row with that value
United_states$Debt[1] <- first
View(United_states)
#-------------
#Find the mean value of each column that has NA
VietnamwithNA
first = mean(Vietnam$GDP, na.rm = TRUE, dims = 1)
second = mean(Vietnam$Real_interest_rate, na.rm = TRUE, dims = 1)
third = mean(Vietnam$Gross_Savings, na.rm = TRUE, dims = 1)
four = mean(Vietnam$Net_Export, na.rm = TRUE, dims = 1)
five = mean(Vietnam$Government_Expenditure, na.rm = TRUE, dims = 1)
six = mean(Vietnam$Debt, na.rm = TRUE, dims = 1)


#Replace First row with that value
Vietnam$GDP[1] <- first
Vietnam$Real_interest_rate[1] <- second
Vietnam$Gross_Savings[1] <- third
Vietnam$Net_Export[1] <- four
Vietnam$Government_Expenditure[1] <- five
Vietnam$Debt[1] <- six

View(Vietnam)

#########################################################################
##Fill in the rest of the missing values using interpolation
# <GDP40> 
Brazilfinal <- na_seadec(Brazil)
View(Brazilfinal)

USfinal <- na_seadec(United_states)
View(USfinal)

Vietnamfinal <- na_seadec(Vietnam)
#Visualize Imputations
ggplot_na_imputations(x_with_na = Vietnam$GDP, 
                      x_with_imputations = Vietnamfinal$GDP,
                      color_imputations = "gold")
View(Vietnamfinal)

#Combine the dataframes together
total <- rbind(Brazilfinal, USfinal)
finaldataset <- rbind(total, Vietnamfinal)

#Add (%) to columns' names that use % 
colnames(finaldataset)[which(names(finaldataset) == "Real_interest_rate")] = "Real_interest_rate (%)"
colnames(finaldataset)[which(names(finaldataset) == "Gross_Savings")] = "Gross_Savings (%)"
colnames(finaldataset)[which(names(finaldataset) == "Debt")] = "Debt (%)"
colnames(finaldataset)[which(names(finaldataset) == "Investment")] = "Investment (%)"

#Show Government_Expenditure, Net_Export in billions
finaldataset[, 'Government_Expenditure (billions)' := Government_Expenditure/1000000000]
finaldataset[, 'Net_Export (billions)' := Net_Export/1000000000]
finaldataset[, 'GDP (billions)' := GDP/1000000000]


#Drop unnecessary clumns
finaldataset[, c('iso2c','GDP','Government_Expenditure','Net_Export') := NULL]

View(finaldataset)

write.csv(finaldataset, file="GDP40_cleaned.csv", row.names = FALSE)

# ==============================================================================================================
#Data Cleaning (Handling Duplicated values)

#<GDP>
gdp <- fread("GDP40_cleaned.csv")
gdp <- gdp[,c(3:10)]
View(gdp)
Brazil <- gdp[1:40,]
United_states <- gdp[41:80]
Vietnam <- gdp[81:120]

table(Brazil$Life_expectancy_at_birth)
table(Brazil$`Real_interest_rate (%)`)
table(Brazil$`Gross_Savings (%)`)
table(Brazil$`Debt (%)`)
table(Brazil$`Investment (%)`)
table(Brazil$`Government_Expenditure (billions)`)
table(Brazil$`Net_Export (billions)`)
table(Brazil$`GDP (billions)`)
#No duplicated values in Brazil

table(United_states$Life_expectancy_at_birth) # 2 duplicated values #2 consecutive years same value
table(United_states$`Real_interest_rate (%)`)
table(United_states$`Gross_Savings (%)`)
table(United_states$`Debt (%)`)
table(United_states$`Investment (%)`)
table(United_states$`Government_Expenditure (billions)`)
table(United_states$`Net_Export (billions)`)
table(United_states$`GDP (billions)`)

table(Vietnam$Life_expectancy_at_birth)
table(Vietnam$`Real_interest_rate (%)`)
table(Vietnam$`Gross_Savings (%)`)
table(Vietnam$`Debt (%)`)
table(Vietnam$`Investment (%)`) #1 duplicate values
table(Vietnam$`Government_Expenditure (billions)`)
table(Vietnam$`Net_Export (billions)`)
table(Vietnam$`GDP (billions)`)

# <CPI>
CPI <- CPI[,c(3:11)]
View(CPI)
United_states <- CPI[1:20,]
Brazil <- CPI[21:40]
Vietnam <- CPI[41:60]

table(Brazil$CPI) 
table(Brazil$PPI) 
table(Brazil$Share_Prices)
table(Brazil$Interest_Rate)
table(Brazil$GDP_per_capita)
table(Brazil$Exchange_Rate) 
table(Brazil$Unemployment) 
table(Brazil$Expenditure)
table(Brazil$Manufacturing) 
#No duplicate values

table(United_states$CPI)
table(United_states$PPI) 
table(United_states$Share_Prices) 
table(United_states$Interest_Rate)
table(United_states$GDP_per_capita)
table(United_states$Exchange_Rate) # 1 duplicated value 
table(United_states$Unemployment)# 3 duplicated value
table(United_states$Expenditure)
table(United_states$Manufacturing) # 2 duplicated value

table(Vietnam$CPI) 
table(Vietnam$PPI) 
table(Vietnam$Share_Prices)
table(Vietnam$Interest_Rate)
table(Vietnam$GDP_per_capita)
table(Vietnam$Exchange_Rate)
table(Vietnam$Unemployment) # 2 duplicated value
table(Vietnam$Expenditure)
table(Vietnam$Manufacturing)
