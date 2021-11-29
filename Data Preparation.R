# ==============================================================================================================
#Required Libraries
library(data.table)
library(dplyr)
library(tidyr)
library(WDI)
library(caTools)
library(car)
library(corrplot)
library(wbstats)

# ==============================================================================================================
#Preparing GDP data for 40 years
GDPWDIraw<- WDI(country=c("US","BR","VN")
                , indicator=c("NY.GDP.MKTP.CD","SP.DYN.LE00.IN", "FR.INR.RINR","NY.GNS.ICTR.ZS","BN.GSR.GNFS.CD","NE.CON.GOVT.CD")
                , start=1980, end=2019)
colnames(GDPWDIraw) <- c("iso2c","country", "year","GDP","Life_expectancy_at_birth","Real_interest_rate","Gross_Savings","Net_Export","Government_Expenditure")
summary(GDPWDIraw)

rawdata<- fread('data.csv')
GDPdebt<- rawdata[rawdata$`Indicator Id` =="2787"]
GDPdebt2<- GDPdebt[GDPdebt$`Country Name`=="United States" 
                   | GDPdebt$`Country Name`=="Brazil"
                   | GDPdebt$`Country Name`=="Vietnam"]

GDPdebt3<- select(GDPdebt2,"Country Name","1980","1981","1982","1983","1984","1985"
                  ,"1986","1987","1988","1989","1990","1991","1992","1993","1994","1995"
                  ,"1996","1997","1998","1999","2000","2001","2002","2003","2004","2005"
                  ,"2006","2007","2008","2009","2010","2011","2012","2013","2014"
                  ,"2015","2016","2017","2018","2019")


GDPdebt4<- GDPdebt3 %>% gather(year,Debt,"1980":"2019")
GDPdebtfinal<- GDPdebt4[order(GDPdebt4$`Country Name`),]
colnames(GDPdebtfinal) <- c("country","year","Debt")

GDPinvestment<- rawdata[rawdata$`Indicator Id` =="345"]
GDPinvestment2<- GDPinvestment[GDPinvestment$`Country Name`=="United States" 
                               | GDPinvestment$`Country Name`=="Brazil"
                               | GDPinvestment$`Country Name`=="Vietnam"]

GDPinvestment3<- select(GDPinvestment2,"Country Name","1980","1981","1982","1983","1984","1985"
                        ,"1986","1987","1988","1989","1990","1991","1992","1993","1994","1995"
                        ,"1996","1997","1998","1999","2000","2001","2002","2003","2004","2005"
                        ,"2006","2007","2008","2009","2010","2011","2012","2013","2014"
                        ,"2015","2016","2017","2018","2019")

GDPinvestment4<- GDPinvestment3 %>% gather(year,Debt,"1980":"2019")
GDPinvestmentfinal<- GDPinvestment4[order(GDPinvestment4$`Country Name`),]
colnames(GDPinvestmentfinal) <- c("country","year","Investment")

GDPdata<-merge(GDPWDIraw,GDPdebtfinal,by=c("country","year"))
GDPdata<-merge(GDPdata,GDPinvestmentfinal,by=c("country","year"))
write.csv(GDPdata,file = "GDPdata_40years.csv",row.names=FALSE)

