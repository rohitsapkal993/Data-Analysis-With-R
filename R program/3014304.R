

# Package 

library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(DataExplorer)
library(tidyverse)
library(dplyr)
library(modeest)
library(Hmisc)
library(e1071)
library(MonoInc)

# Data Loading

Ice <- read.csv("https://raw.githubusercontent.com/sedaerdem/Statistics/master/data/icecream.csv")

IceA <- as_data_frame(Ice)
IceB <- as_data_frame(Ice)

# (A) EDA

# Filter by Country

IceA %>%
  filter(country  =='A')

IceB %>%
  filter(country  =='B')

# Filter by Seasons

IceS %>%
  filter(seasons  =='Summer')

IceA %>%
  filter(seasons  =='Autumn')

IceW %>%
  filter(seasons  =='Winter')

IceSp %>%
  filter(seasons  =='Spring')

# Summarise

dim(IceA)
dim(IceB)
dim(Ice)
summary(IceA)
summary(IceB)
summary(Ice)

# Discriptive Statistics Full Numeric EDA

# Mode

modeP = mfv(Ice$price)
print(modeP)

modeSID = mfv(Ice$ShopID)
print(modeSID)

modeT = mfv(Ice$temperature)
print(modeT)

modeIN = mfv(Ice$income)
print(modeIN)

modeICS = mfv(Ice$icecream_sales)
print(modeICS)

mode = mfv(Ice$seasons)
print(mode)

modeC = mfv(Ice$country)
print(modeC)

# percentile

durationSID = Ice$ShopID 
quantile(durationSID, c(.05, .57, .95)) 

durationP = Ice$price 
quantile(durationP, c(.05, .57, .95)) 

durationIS = Ice$icecream_sales 
quantile(durationIS, c(.05, .57, .95)) 

durationIN = Ice$income 
quantile(durationIN, c(.05, .57, .95)) 

durationT = Ice$temperature 
quantile(durationT, c(.05, .57, .95)) 

# Range

RangeS = Ice$ShopID
max(RangeS) - min(RangeS) 

RangeIN = Ice$income
max(RangeIN) - min(RangeIN) 

RangeT = Ice$temperature
max(RangeT) - min(RangeT) 

RangeP = Ice$price
max(RangeP) - min(RangeP) 

RangeICS = Ice$icecream_sales
max(RangeICS) - min(RangeICS) 

# Inter Quartile Range

IQRT = Ice$temperature 
IQR(IQRT)

IQRI = Ice$income 
IQR(IQRI)

IQRP = Ice$price 
IQR(IQRP)

IQRICS = Ice$icecream_sales 
IQR(IQRICS)

IQRS = Ice$ShopID 
IQR(IQRS)

# Standard deviation

SDS = Ice$ShopID 
sd(SDS)

SDT = Ice$temperature 
sd(SDT)

SDI = Ice$income 
sd(SDI)

SDP = Ice$price 
sd(SDP)

SDICS = Ice$icecream_sales 
sd(SDICS)

# Coefficients of variance

CVI = Ice$icecream_sales
cvi <- sd(CVI) / mean(CVI)
cvi

CVS = Ice$ShopID
cvs <- sd(CVS) / mean(CVS)
cvs

CVT = Ice$temperature
cvt <- sd(CVT) / mean(CVT)
cvt

CVIN = Ice$income
cvin <- sd(CVIN) / mean(CVIN)
cvin

CVP = Ice$price
cvp <- sd(CVP) / mean(CVP)
cvp

# Kurtosis

KID = Ice$ShopID
kurtosis(KID)

KIT = Ice$temperature
kurtosis(KIT)

KIP = Ice$price
kurtosis(KIP)

KII = Ice$income
kurtosis(KII)

KIICS = Ice$icecream_sales
kurtosis(KIICS)

# Skewness

skewness(Ice$ShopID) 

skewness(Ice$temperature)

skewness(Ice$income)

skewness(Ice$icecream_sales)

skewness(Ice$price)

# Variance

var(Ice$ShopID)

var(Ice$temperature)

var(Ice$price)

var(Ice$income)

var(Ice$icecream_sales)

# Monotonic

monotonic(Ice$ShopID)

monotonic(Ice$temperature)

monotonic(Ice$income)

monotonic(Ice$icecream_sales)

monotonic(Ice$income)

monotonic(Ice$country)

monotonic(Ice$seasons)

# Visualisation

#Histograms

#Income

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=income,fill = country)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('Income Visualas') +
  facet_wrap(~country)

# Income Density

Ice %>%
  filter(country =='A' | country == 'B') %>%
  ggplot(aes(x=income, fill = country)) +
  geom_density(alpha=0.5, color='darkblue') +
  ggtitle('Income density') +
  facet_wrap(~country)

# Price hist

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=price,fill=country)) +
  geom_histogram(alpha=0.5, color='darkblue') +
  ggtitle('Price Visualas') +
  facet_wrap(~country)

# Price density

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=price,fill=country)) +
  geom_density(alpha=0.5, color='darkblue') +
  ggtitle('Price Visualas') +
  facet_wrap(~country)

#Ice cream sales hist

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=icecream_sales,fill= country)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('Icecrem Visualas') +
  facet_wrap(~country)

#Ice cream sales density

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=icecream_sales,fill= country)) +
  geom_density(alpha=0.8, color='darkblue') +
  ggtitle('Icecrem Visualas') +
  facet_wrap(~country)

# Temperature hist

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=temperature,fill=country)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('Temp Visualas') +
  facet_wrap(~country)

# Temperaure Density

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=temperature,fill=country)) +
  geom_density(alpha=0.8, color='darkblue') +
  ggtitle('Temp Visualas') +
  facet_wrap(~country)

# Country Hist

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=temperature,fill=country)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('Country Visualas') +
  facet_wrap(~country)

# Country density

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=temperature,fill=country)) +
  geom_density(alpha=0.8, color='darkblue') +
  ggtitle('Country Visualas') +
  facet_wrap(~country)

# Shop ID hist

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=ShopID,fill=country)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('ShopID') +
  facet_wrap(~country)

#Shop ID Density

Ice %>%
  filter(country=='A' | country == 'B') %>%
  ggplot(aes(x=ShopID,fill=country)) +
  geom_density(alpha=0.8, color='darkblue') +
  ggtitle('ShopID') +
  facet_wrap(~country)

'''
# Seasons
#Ice %>%
 #filter(seasons=='Autumn' | country == 'Winter') %>%
  #ggplot(aes(x=temperature,fill=seasons)) +
  #geom_histogram(alpha=0.8, color='darkblue') +
  #ggtitle('Seasons Visualas') +
  #facet_wrap(~seasons)
'''

#Scatter Plot

pairs(Ice[,1:5],)


# (B) Hypothesis testing 

?t.test

t.test(icecream_sales ~ country , data = Ice, var.equal=TRUE,paired=FALSE)

# (C) Modelling

'''
# Fitting multiple linear regression
#Ice cream_sales is my dependent variable and all other are independent variable
#mlrm <- lm(icecream_sales~., data = Ice) --- this for all the variables
'''
'''
Q.1
'''

mlrm <- lm(icecream_sales~income+price+temperature+country+seasons, data = Ice)
mlrm
summary(mlrm)

'''
Q.2
'''
#Variance analysis

variance <- lm(icecream_sales~income+price+temperature+country+seasons, data = Ice)
full <- lm(icecream_sales~., data = Ice)
anova(variance,full)

'''
#AVG Prediction ice cream sales in a location in Country A with an average income of £20,000 
#and a location in Country B with an average income of £30,000?
#mlrmC <- lm(icecream_sales~income+country+temperature+price+seasons,data=Ice)
'''
'''
Q 3
'''

mlrmC <- lm(icecream_sales~income+country,data=Ice)
confint(mlrmC,level = 0.05)

'''
#I take mean value of temperature,price and most common season
#newdataC = data.frame(income=20000,country = "A",temperature=16.75,price=2.94,seasons="Autumn")
'''
# Country A

newdataC = data.frame(income=20000,country = "A")
d1 = predict(mlrmC,newdataC)
d1

# Country B

newdataCB = data.frame(income=30000,country = "B")
d2 = predict(mlrmC,newdataCB)
d2

# Difference between this two predictions

d3 = d2 - d1
d3

'''
Q.4
'''

# When temperature goes up by 2 Celsius and price goes up by 0.50 GBP without changing the other variables.

mlrmCN <- lm(icecream_sales~temperature+price+country+income,data=Ice)
mlrmCN

# Country A model

newdataCT = data.frame(income=20000,country = "A",temperature=18.75,price=3.44)
D1 = predict(mlrmCN,newdataCT)
D1
Confidence_intervalA = predict(mlrmCN,newdataCT,interval = "confidence",leval=.90)
Confidence_intervalA

# Country B model

newdataCBT = data.frame(income=30000,country = "B",temperature=18.75,price=3.44)
D2 = predict(mlrmCN,newdataCBT)
D2
Confidence_intervalB = predict(mlrmCN,newdataCBT,interval = "confidence",leval=.90)
Confidence_intervalB

# Predicted change

D3 = D2 - D1
D3

'''
Q5
'''
# Variance by this two new model

varianceN <- lm(icecream_sales~income+country,data=Ice)
FullN <- lm(icecream_sales~temperature+price+country+income,data=Ice)
anova(varianceN,FullN)


'''
Q6
'''
# confidence interval for the regression coefficient at 5% significant level

confint(mlrmCN,level = 0.05)

'''
Q7
'''
# Confidence interval using 90% of confidence level on ice cream sales

t.test(Ice$icecream_sales, conf.level = 0.90)
T = 549.7392 - 532.926
T

#qt(.975,999)*sd(Ice$icecream_sales)/sqrt(1000)


# (D) Prediction

mlrm <- lm(icecream_sales~income+price+temperature+country+seasons, data = Ice)
Predicted_newdata=data.frame(income = 30000, temperature=23,price = 3,country = "A",seasons="Spring", conf.level = .95)
predict(mlrm,Predicted_newdata)

ci=predict(mlrm,Predicted_newdata,interval = "confidence",leval=.95)
ci





# To generate full EDA report 

Ice %>% glimpse()
Ice %>%
 create_report(
 output_file = "Ice_report 1",
 output_dir = "Eda_Ice/",
 report_title = "Eda Report_Ice"
)





