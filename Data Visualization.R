# ==============================================================================================================
#Required Libraries
library(data.table)
library(ggplot2)
library(corrgram)
install.packages("cowplot")
library(cowplot)
library(corrplot)
install.packages("pastecs")
library(pastecs)
install.packages("xtable")
library(xtable)
library(car)
library(corrplot)
library(caTools)
library(scales)

# ==============================================================================================================
#Importing datasets
GDP <- fread("GDP40_cleaned.csv")
CPI <- fread("CPI_Dataset.csv")

# ==============================================================================================================
#Data Visualization

#   << GDP >>  
View(GDP)
summary(GDP)

Brazil_GDP <- GDP[country=="Brazil"]
USA_GDP <- GDP[country=="United States"]
Vietnam_GDP <- GDP[country=="Vietnam"]

#Statistic Summaries
summary(Brazil_GDP)
summary(USA_GDP)
summary(Vietnam_GDP)
stat.desc(Brazil_GDP)
stat.desc(USA_GDP)
stat.desc(Vietnam_GDP)

#ggplots for GDP
bw.nrd0(Brazil_GDP$`GDP (billions)`) #default bandwidth
ggplot(Brazil_GDP, aes(x = `GDP (billions)`)) +
  geom_density(fill = "deepskyblue", 
               bw = 50) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 50")

bw.nrd0(USA_GDP$`GDP (billions)`) #default bandwidth
ggplot(USA_GDP, aes(x = `GDP (billions)`)) +
  geom_density(fill = "deepskyblue", 
               bw = 300) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 300")

bw.nrd0(Vietnam_GDP$`GDP (billions)`) #default bandwidth
ggplot(Vietnam_GDP, aes(x = `GDP (billions)`)) +
  geom_density(fill = "deepskyblue", 
               bw = 5) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 5")


# Relation between year & GDP
plot(Brazil_GDP$year, Brazil_GDP$GDP, type = "o", col = "red", lwd = 2,
     main = "Relationship between Brazil GDP & Year",
     xlab = "YEAR", ylab = "Brazil GDP in US$",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

plot(USA_GDP$year, USA_GDP$GDP, type = "o", col = "red", lwd = 2,
     main = "Relationship between USA GDP & Year",
     xlab = "YEAR", ylab = "USA GDP in US$",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

plot(Vietnam_GDP$year, Vietnam_GDP$GDP, type = "o", col = "red", lwd = 2,
     main = "Relationship between Vietnam GDP & Year",
     xlab = "YEAR", ylab = "Vietnam GDP in US$",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

#Scatterplot of Variables against time
##Brazil
gdp <- ggplot(Brazil_GDP, aes(x= year, y=`GDP (billions)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
life <- ggplot(Brazil_GDP, aes(x= year, y= Life_expectancy_at_birth)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(Brazil_GDP, aes(x= year, y= `Real_interest_rate (%)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
savings <- ggplot(Brazil_GDP, aes(x= year, y= `Gross_Savings (%)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
export <- ggplot(Brazil_GDP, aes(x= year, y=`Net_Export (billions)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
gov <- ggplot(Brazil_GDP, aes(x= year, y= `Government_Expenditure (billions)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
debt <- ggplot(Brazil_GDP, aes(x= year, y=`Debt (%)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
invest <- ggplot(Brazil_GDP, aes(x= year, y= `Investment (%)`)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())

plot_grid(gdp, life, IR, ncol = 1, labels = c("", "", ""))
plot_grid(savings, export, gov, ncol = 1, labels = c("", "", ""))
plot_grid(debt, invest, ncol = 1, labels = c("", ""))

##US
gdp <- ggplot(USA_GDP, aes(x= year, y=`GDP (billions)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1)+ theme(axis.text.y=element_blank())
life <- ggplot(USA_GDP, aes(x= year, y= Life_expectancy_at_birth)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(USA_GDP, aes(x= year, y= `Real_interest_rate (%)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
savings <- ggplot(USA_GDP, aes(x= year, y= `Gross_Savings (%)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
export <- ggplot(USA_GDP, aes(x= year, y=`Net_Export (billions)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
gov <- ggplot(USA_GDP, aes(x= year, y= `Government_Expenditure (billions)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
debt <- ggplot(USA_GDP, aes(x= year, y=`Debt (%)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())
invest <- ggplot(USA_GDP, aes(x= year, y= `Investment (%)`)) +
  geom_point() + geom_line(color = "#FC4E07", size = 1) + theme(axis.text.y=element_blank())

plot_grid(gdp, life, IR, ncol = 1, labels = c("", "", ""))
plot_grid(savings, export, gov, ncol = 1, labels = c("", "", ""))
plot_grid(debt, invest, ncol = 1, labels = c("", ""))

##Vietnam
gdp <- ggplot(Vietnam_GDP, aes(x= year, y=`GDP (billions)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
life <- ggplot(Vietnam_GDP, aes(x= year, y= Life_expectancy_at_birth)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(Vietnam_GDP, aes(x= year, y= `Real_interest_rate (%)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
savings <- ggplot(Vietnam_GDP, aes(x= year, y= `Gross_Savings (%)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
export <- ggplot(Vietnam_GDP, aes(x= year, y=`Net_Export (billions)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
gov <- ggplot(Vietnam_GDP, aes(x= year, y= `Government_Expenditure (billions)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
debt <- ggplot(Vietnam_GDP, aes(x= year, y=`Debt (%)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())
invest <- ggplot(Vietnam_GDP, aes(x= year, y= `Investment (%)`)) +
  geom_point() + geom_line(color = "#E7B800", size = 1) + theme(axis.text.y=element_blank())

plot_grid(gdp, life, IR, ncol = 1, labels = c("", "", ""))
plot_grid(savings, export, gov, ncol = 1, labels = c("", "", ""))
plot_grid(debt, invest, ncol = 1, labels = c("", ""))



#To use corrplot, need to drop country columm 
corr_Brazil <- Brazil_GDP[,c(2:10)]
corr_USA <- USA_GDP[,c(2:10)]
corr_Vietnam <- Vietnam_GDP[,c(2:10)]

#Correlation between year and GDP
cor(Brazil_GDP$`GDP (billions)`, Brazil_GDP$year)
cor(USA_GDP$`GDP (billions)`, USA_GDP$year)
cor(Vietnam_GDP$`GDP (billions)`, Vietnam_GDP$year)

#Correlation between GDP and all X Variables
##Brazil
cor(corr_Brazil)
cor(Brazil_GDP$GDP, Brazil_GDP$year)
cor(Brazil_GDP$GDP, Brazil_GDP$Life_expectancy_at_birth)
cor(Brazil_GDP$GDP, Brazil_GDP$Real_interest_rate)
cor(Brazil_GDP$GDP, Brazil_GDP$Gross_Savings)
cor(Brazil_GDP$GDP, Brazil_GDP$Net_Export)
cor(Brazil_GDP$GDP, Brazil_GDP$Government_Expenditure)
cor(Brazil_GDP$GDP, Brazil_GDP$Debt)
cor(Brazil_GDP$GDP, Brazil_GDP$Investment)

##USA
cor(corr_USA)
cor(USA_GDP$GDP, USA_GDP$year)
cor(USA_GDP$GDP, USA_GDP$Life_expectancy_at_birth)
cor(USA_GDP$GDP, USA_GDP$Real_interest_rate)
cor(USA_GDP$GDP, USA_GDP$Gross_Savings)
cor(USA_GDP$GDP, USA_GDP$Net_Export)
cor(USA_GDP$GDP, USA_GDP$Government_Expenditure)
cor(USA_GDP$GDP, USA_GDP$Debt)
cor(USA_GDP$GDP, USA_GDP$Investment)

##Vietnam
cor(corr_Vietnam)
cor(Vietnam_GDP$GDP, Vietnam_GDP$year)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Life_expectancy_at_birth)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Real_interest_rate)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Gross_Savings)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Net_Export)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Government_Expenditure)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Debt)
cor(Vietnam_GDP$GDP, Vietnam_GDP$Investment)

pairs(corr_Brazil)
pairs(~ `GDP (billions)` + year + Life_expectancy_at_birth + `Real_interest_rate (%)`, data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "year", "Life Expectancy", "Real Interest Rate"))

pairs(~ `GDP (billions)` + `Gross_Savings (%)` + `Net_Export (billions)` + `Government_Expenditure (billions)`, data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

pairs(~ `GDP (billions)` + `Debt (%)` + corr_Brazil$`Investment (%)` , data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

pairs(corr_USA)
pairs(~ `GDP (billions)` + year + Life_expectancy_at_birth + `Real_interest_rate (%)`, data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "year", "Life Expectancy", "Real Interest Rate"))

pairs(~ `GDP (billions)` + `Gross_Savings (%)` + `Net_Export (billions)` + `Government_Expenditure (billions)`, data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

pairs(~ `GDP (billions)` + `Debt (%)` + corr_Brazil$`Investment (%)` , data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

pairs(corr_Vietnam)
pairs(~ `GDP (billions)` + year + Life_expectancy_at_birth + `Real_interest_rate (%)`, data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "year", "Life Expectancy", "Real Interest Rate"))

pairs(~ `GDP (billions)` + `Gross_Savings (%)` + `Net_Export (billions)` + `Government_Expenditure (billions)`, data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

pairs(~ `GDP (billions)` + `Debt (%)` + corr_Brazil$`Investment (%)` , data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("GDP", "Savings", "Export", "Govt Expenditure"))

#Correlation Plot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
##Brazil
corrplot(cor(corr_Brazil), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Brazil Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))
##USA
corrplot(cor(corr_USA), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of USA Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

##Vietnam
corrplot(cor(corr_Vietnam), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Vietnam Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

#----------------------------------
#   << CPI >>
View(CPI)
summary(CPI)

Brazil_CPI <- CPI[country=="Brazil"]
USA_CPI <- CPI[country=="United States"]
Vietnam_CPI <- CPI[country=="Vietnam"]

#Statistic Summaries
summary(Brazil_CPI)
summary(USA_CPI)
summary(Vietnam_CPI)
stat.desc(Brazil_CPI)
stat.desc(USA_CPI)
stat.desc(Vietnam_CPI)

# Relation between year & CPI
plot(Brazil_CPI$year, Brazil_CPI$CPI, type = "o", col = "red", lwd = 2,
     main = "Relationship between Brazil CPI & Year",
     xlab = "YEAR", ylab = "Brazil CPI",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

plot(USA_CPI$year, USA_CPI$CPI, type = "o", col = "red", lwd = 2,
     main = "Relationship between USA CPI & Year",
     xlab = "YEAR", ylab = "USA CPI",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

plot(Vietnam_CPI$year, Vietnam_CPI$CPI, type = "o", col = "red", lwd = 2,
     main = "Relationship between Vietnam CPI & Year",
     xlab = "YEAR", ylab = "Vietnam CPI",
     col.main="black", col.lab="blue",
     cex.main=1.5, cex.lab=1.2)

#Scatterplot of Variables against time
##Brazil
cpi <- ggplot(Brazil_CPI, aes(x= year, y=CPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
ppi <- ggplot(Brazil_CPI, aes(x= year, y= PPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
share <- ggplot(Brazil_CPI, aes(x= year, y= Share_Prices)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(Brazil_CPI, aes(x= year, y= Interest_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
gdp <- ggplot(Brazil_CPI, aes(x= year, y= GDP_per_capita)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
er <- ggplot(Brazil_CPI, aes(x= year, y= Exchange_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
unemployment <- ggplot(Brazil_CPI, aes(x= year, y= Unemployment)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
expenditure <- ggplot(Brazil_CPI, aes(x= year, y= Expenditure)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
manufacture <- ggplot(Brazil_CPI, aes(x= year, y= Brazil_CPI$Manufacturing)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())

plot_grid(cpi, ppi, share, ncol = 1, labels = c("", "", ""))
plot_grid(IR, gdp, er, ncol = 1, labels = c("", "", ""))
plot_grid(unemployment, expenditure, manufacture, ncol = 1, labels = c("", ""))

##US
cpi <- ggplot(USA_CPI, aes(x= year, y=CPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
ppi <- ggplot(USA_CPI, aes(x= year, y= PPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
share <- ggplot(USA_CPI, aes(x= year, y= Share_Prices)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(USA_CPI, aes(x= year, y= Interest_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
gdp <- ggplot(USA_CPI, aes(x= year, y= GDP_per_capita)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
er <- ggplot(USA_CPI, aes(x= year, y= Exchange_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
unemployment <- ggplot(USA_CPI, aes(x= year, y= Unemployment)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
expenditure <- ggplot(USA_CPI, aes(x= year, y= Expenditure)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
manufacture <- ggplot(USA_CPI, aes(x= year, y= Brazil_CPI$Manufacturing)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())

plot_grid(cpi, ppi, share, ncol = 1, labels = c("", "", ""))
plot_grid(IR, gdp, er, ncol = 1, labels = c("", "", ""))
plot_grid(unemployment, expenditure, manufacture, ncol = 1, labels = c("", ""))

##Vietnam
cpi <- ggplot(Vietnam_CPI, aes(x= year, y=CPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
ppi <- ggplot(Vietnam_CPI, aes(x= year, y= PPI)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
share <- ggplot(Vietnam_CPI, aes(x= year, y= Share_Prices)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
IR <- ggplot(Vietnam_CPI, aes(x= year, y= Interest_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
gdp <- ggplot(Vietnam_CPI, aes(x= year, y= GDP_per_capita)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
er <- ggplot(Vietnam_CPI, aes(x= year, y= Exchange_Rate)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
unemployment <- ggplot(Vietnam_CPI, aes(x= year, y= Unemployment)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
expenditure <- ggplot(Vietnam_CPI, aes(x= year, y= Expenditure)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())
manufacture <- ggplot(Vietnam_CPI, aes(x= year, y= Brazil_CPI$Manufacturing)) +
  geom_point() + geom_line(color = "#00AFBB", size = 1) + theme(axis.text.y=element_blank())

plot_grid(cpi, ppi, share, ncol = 1, labels = c("", "", ""))
plot_grid(IR, gdp, er, ncol = 1, labels = c("", "", ""))
plot_grid(unemployment, expenditure, manufacture, ncol = 1, labels = c("", ""))


#To use corrplot, need to drop country columm 
corr_Brazil <- Brazil_CPI[,c(2:11)]
corr_USA <- USA_CPI[,c(2:11)]
corr_Vietnam <- Vietnam_CPI[,c(2:11)]

#Correlation between year and GDP
cor(Brazil_CPI$CPI, Brazil_CPI$year)
cor(USA_CPI$CPI, USA_CPI$year)
cor(Vietnam_CPI$CPI, Vietnam_CPI$year)

#Correlation between GDP and all X Variables
##Brazil
cor(corr_Brazil)
cor(Brazil_CPI$CPI, Brazil_CPI$year)
cor(Brazil_CPI$CPI, Brazil_CPI$PPI)
cor(Brazil_CPI$CPI, Brazil_CPI$Share_Prices)
cor(Brazil_CPI$CPI, Brazil_CPI$Interest_Rate)
cor(Brazil_CPI$CPI, Brazil_CPI$GDP_per_capita)
cor(Brazil_CPI$CPI, Brazil_CPI$Exchange_Rate)
cor(Brazil_CPI$CPI, Brazil_CPI$Unemployment)
cor(Brazil_CPI$CPI, Brazil_CPI$Expenditure)
cor(Brazil_CPI$CPI, Brazil_CPI$Manufacturing)
##USA
cor(corr_USA)
cor(USA_CPI$CPI, USA_CPI$year)
cor(USA_CPI$CPI, USA_CPI$PPI)
cor(USA_CPI$CPI, USA_CPI$Share_Prices)
cor(USA_CPI$CPI, USA_CPI$Interest_Rate)
cor(USA_CPI$CPI, USA_CPI$GDP_per_capita)
cor(USA_CPI$CPI, USA_CPI$Exchange_Rate)
cor(USA_CPI$CPI, USA_CPI$Unemployment)
cor(USA_CPI$CPI, USA_CPI$Expenditure)
cor(USA_CPI$CPI, USA_CPI$Manufacturing)
##Vietnam
cor(corr_Vietnam)
cor(Vietnam_CPI$CPI, Vietnam_CPI$year)
cor(Vietnam_CPI$CPI, Vietnam_CPI$PPI)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Share_Prices)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Interest_Rate)
cor(Vietnam_CPI$CPI, Vietnam_CPI$GDP_per_capita)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Exchange_Rate)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Unemployment)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Expenditure)
cor(Vietnam_CPI$CPI, Vietnam_CPI$Manufacturing)

pairs(corr_Brazil)
pairs(~ CPI + year + PPI + Share_Prices, data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "year", "PPI", "Share_Prices"))

pairs(~ CPI + Interest_Rate + GDP_per_capita + Exchange_Rate, data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Interest Rate", "GDP per capita", "Share prices"))

pairs(~ CPI + Unemployment + Expenditure + Manufacturing, data = corr_Brazil
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Unemployment", " Expenditure", "Manufacturing"))

pairs(corr_USA)
pairs(~ CPI + year + PPI + Share_Prices, data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "year", "PPI", "Share_Prices"))

pairs(~ CPI + Interest_Rate + GDP_per_capita + Exchange_Rate, data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Interest Rate", "GDP per capita", "Share prices"))

pairs(~ CPI + Unemployment + Expenditure + Manufacturing, data = corr_USA
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Unemployment", " Expenditure", "Manufacturing"))

pairs(corr_Vietnam)
pairs(~ CPI + year + PPI + Share_Prices, data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "year", "PPI", "Share_Prices"))

pairs(~ CPI + Interest_Rate + GDP_per_capita + Exchange_Rate, data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Interest Rate", "GDP per capita", "Share prices"))

pairs(~ CPI + Unemployment + Expenditure + Manufacturing, data = corr_Vietnam
      , col = "red"
      , pch = 18
      ,labels = c("CPI", "Unemployment", " Expenditure", "Manufacturing"))


#Correlation Plot
##Brazil
corrplot(cor(corr_Brazil), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Brazil Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))

##USA
corrplot(cor(corr_USA), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of USA Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))
##Vietnam
corrplot(cor(corr_Vietnam), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of Vietnam Dataset Variables",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))



